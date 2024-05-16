use crate::{
    events::{Op, TargetEvent},
    relation::{CleanupPolicy, Relation, RelationId, ZstOrPanic},
};

use bevy_derive::{Deref, DerefMut};
use bevy_ecs::{
    component::{Component, ComponentHooks, ComponentId, StorageType},
    entity::Entity,
    query::{AnyOf, Changed, Or, QueryData, QueryFilter, With, Without},
    system::EntityCommands,
    world::{Command, DeferredWorld, EntityWorldMut, World},
};
use bevy_hierarchy::{Children, Parent};
use bevy_log::warn;

use smallvec::SmallVec;
use std::marker::PhantomData;

// Small Stable Unique Vec
pub(crate) struct SSUVec<T: PartialEq> {
    pub vec: SmallVec<[T; 1]>,
}

impl<T: PartialEq> Default for SSUVec<T> {
    fn default() -> Self {
        Self {
            vec: SmallVec::default(),
        }
    }
}

impl<T: PartialEq> SSUVec<T> {
    pub(crate) fn add(&mut self, val: T) {
        if self.vec.iter().all(|item| *item != val) {
            self.vec.push(val)
        }
    }

    pub(crate) fn remove(&mut self, val: T) {
        if let Some(n) = self
            .vec
            .iter()
            .enumerate()
            .find_map(|(n, item)| (*item == val).then_some(n))
        {
            self.vec.remove(n);
        }
    }
}

// For cleanup types:
// - Orphan
// - Counted
pub(crate) fn unset_edges<R: Relation>(mut world: DeferredWorld, id: Entity, _: ComponentId) {
    let hosts = world
        .get_mut::<Hosts<R>>(id)
        .map(|mut hosts| std::mem::take(&mut hosts.vec.vec))
        .unwrap_or_default();

    let targets = world
        .get_mut::<Targets<R>>(id)
        .map(|mut targets| std::mem::take(&mut targets.vec.vec))
        .unwrap_or_default();

    let mut cmds = world.commands();

    for host in hosts.iter().copied() {
        cmds.add(UnsetAsymmetric::<R>::buffered(host, id));
    }

    for target in targets.iter().copied() {
        cmds.add(UnsetAsymmetric::<R>::buffered(id, target));
    }
}

// For cleanup types:
// - Recrusive
// - Total
pub(crate) fn clean_recursive<R: Relation>(mut world: DeferredWorld, id: Entity, _: ComponentId) {
    let hosts = world
        .get_mut::<Hosts<R>>(id)
        .map(|mut edges| std::mem::take(&mut edges.vec.vec))
        .unwrap_or_default();

    let targets = world
        .get_mut::<Targets<R>>(id)
        .map(|mut edges| std::mem::take(&mut edges.vec.vec))
        .unwrap_or_default();

    let mut cmds = world.commands();

    for host in hosts.iter().copied() {
        cmds.add(move |world: &mut World| {
            world.despawn(host);
        });
    }

    for target in targets.iter().copied() {
        cmds.add(UnsetAsymmetric::<R>::buffered(id, target));
    }
}

#[derive(Deref, DerefMut)]
pub(crate) struct Hosts<R: Relation> {
    #[deref]
    pub(crate) vec: SSUVec<Entity>,
    _phantom: PhantomData<R>,
}

impl<R: Relation> Default for Hosts<R> {
    fn default() -> Self {
        Self {
            vec: SSUVec::default(),
            _phantom: PhantomData,
        }
    }
}

impl<R: Relation> Component for Hosts<R> {
    const STORAGE_TYPE: StorageType = StorageType::Table;
    fn register_component_hooks(hooks: &mut ComponentHooks) {
        hooks.on_remove(match R::CLEANUP_POLICY {
            CleanupPolicy::Orphan | CleanupPolicy::Counted => unset_edges::<R>,
            CleanupPolicy::Recursive | CleanupPolicy::Total => clean_recursive::<R>,
        });
    }
}

#[derive(Deref, DerefMut)]
pub(crate) struct Targets<R: Relation> {
    #[deref]
    pub(crate) vec: SSUVec<Entity>,
    _phantom: PhantomData<R>,
}

impl<R: Relation> Default for Targets<R> {
    fn default() -> Self {
        Self {
            vec: SSUVec::default(),
            _phantom: PhantomData,
        }
    }
}

impl<R: Relation> Component for Targets<R> {
    const STORAGE_TYPE: StorageType = StorageType::Table;
    fn register_component_hooks(hooks: &mut ComponentHooks) {
        hooks.on_remove(match R::CLEANUP_POLICY {
            CleanupPolicy::Orphan | CleanupPolicy::Counted => unset_edges::<R>,
            CleanupPolicy::Recursive | CleanupPolicy::Total => clean_recursive::<R>,
        });
    }
}

#[allow(missing_docs)]
pub type EdgeIter<'a> = std::iter::Copied<std::slice::Iter<'a, Entity>>;

/// Edges world query for hierarchy compatibility
#[derive(QueryData)]
pub struct HierarchyEdges(pub(crate) AnyOf<(&'static Children, &'static Parent)>);

/// World query to get the edge info of a Relation.
#[derive(QueryData)]
pub struct Edges<R: Relation>(pub(crate) AnyOf<(&'static Hosts<R>, &'static Targets<R>)>);

/// Get information from a single edge bucket.
pub trait EdgeInfo {
    /// Get all hosts.
    fn hosts(&self) -> &[Entity];
    /// Get all targets.
    fn targets(&self) -> &[Entity];
}

impl EdgeInfo for HierarchyEdgesItem<'_> {
    fn hosts(&self) -> &[Entity] {
        match self {
            Self((Some(hosts), _)) => hosts,
            _ => &[],
        }
    }

    fn targets(&self) -> &[Entity] {
        match self {
            Self((_, Some(target))) => target.as_slice(),
            _ => &[],
        }
    }
}

impl<R: Relation> EdgeInfo for EdgesItem<'_, R> {
    fn hosts(&self) -> &[Entity] {
        match self {
            Self((Some(hosts), _)) => &hosts.vec.vec,
            _ => &[],
        }
    }

    fn targets(&self) -> &[Entity] {
        match self {
            Self((_, Some(targets))) => &targets.vec.vec,
            _ => &[],
        }
    }
}

// Usually bad but needed for operation API glue
impl<E: EdgeInfo> EdgeInfo for Option<E> {
    fn hosts(&self) -> &[Entity] {
        match self {
            Some(edges) => edges.hosts(),
            None => &[],
        }
    }

    fn targets(&self) -> &[Entity] {
        match self {
            Some(edges) => edges.targets(),
            None => &[],
        }
    }
}

/// Filter to find roots of a relationship graph.
/// An entity is a root of `R` if:
/// - It is targeted by atleast one other entity via `R`.
/// - It does not target any other entity via `R`.
#[derive(QueryFilter)]
pub struct Root<R: Relation>((With<Hosts<R>>, Without<Targets<R>>));

/// Filter to find branches of a relationship graph.
/// A branch of `R` has **both** hosts and targets.
#[derive(QueryFilter)]
pub struct Branch<R: Relation>((With<Hosts<R>>, With<Targets<R>>));

/// Filter to find leaves of a relationship graph.
/// An entity is a leaf of `R` if:
/// - It targets atleast 1 other entity via `R`.
/// - It is not targeted by any other entity via `R`.
#[derive(QueryFilter)]
pub struct Leaf<R: Relation>((Without<Hosts<R>>, With<Targets<R>>));

/// Filter to find participants of a relationship.
/// A participant of `R` has **either** hosts or targets.
#[derive(QueryFilter)]
pub struct Participates<R: Relation>(Or<(With<Hosts<R>>, With<Targets<R>>)>);

/// Filter to find entities that do not participante in a relationship.
/// Ie. have no edges comming in or out.
#[derive(QueryFilter)]
pub struct Abstains<R: Relation>((Without<Hosts<R>>, Without<Targets<R>>));

/// Filter to check entities that recently had a relation changed.
#[derive(QueryFilter)]
pub struct EdgeChanged<R: Relation>(Or<(Changed<Hosts<R>>, Changed<Targets<R>>)>);

// Cleanup functions go in both directions to prevent cleanup depending on if a host was
// added/removed first or if a target was added/removed first.

/// Command to set a relationship target for an entity. If either of the participants do not exist
/// or the host tries to target itself the operation will be ignored and logged.
pub struct Set<R>
where
    R: Relation,
{
    host: Entity,
    target: Entity,
    symmetric_action: bool,
    _phantom: PhantomData<R>,
}

impl<R: Relation> Set<R> {
    #[allow(missing_docs)]
    pub fn new(host: Entity, target: Entity) -> Self {
        Self {
            host,
            target,
            symmetric_action: false,
            _phantom: PhantomData,
        }
    }
}

impl<R> Command for Set<R>
where
    R: Relation,
{
    fn apply(self, world: &mut World) {
        let _ = R::ZST_OR_PANIC;

        if self.host == self.target {
            warn!(
                "{host:?} Tried to target to itself with {rel}. \
                Self referential relations are not allowed. \
                Ignoring.",
                host = self.host,
                rel = std::any::type_name::<R>(),
            );
            return;
        }

        if world.get_entity(self.target).is_none() {
            warn!(
                "{host:?} tried to target {target:?} with {rel}. \
                {target:?} does not exist. \
                Ignoring.",
                host = self.host,
                target = self.target,
                rel = std::any::type_name::<R>(),
            );
            return;
        }

        if world.get_entity(self.host).is_none() {
            warn!(
                "{host:?} tried to target {target:?} with {rel}. \
                {host:?} does not exist. \
                Ignoring.",
                host = self.host,
                target = self.target,
                rel = std::any::type_name::<R>(),
            );
            return;
        }

        // add target
        let mut host_targets = world
            .entity_mut(self.host)
            .get_mut::<Targets<R>>()
            .map(|mut targets| std::mem::take(&mut *targets))
            .unwrap_or_default();

        let old = host_targets.vec.vec.first().copied();
        host_targets.add(self.target);
        world.entity_mut(self.host).insert(host_targets);

        // add host
        let mut target_hosts = world
            .entity_mut(self.target)
            .get_mut::<Hosts<R>>()
            .map(|mut hosts| std::mem::take(&mut *hosts))
            .unwrap_or_default();

        target_hosts.vec.add(self.host);
        world.entity_mut(self.target).insert(target_hosts);

        world.send_event(TargetEvent {
            host: self.host,
            target_op: Op::Set,
            target: self.target,
            relation_id: RelationId::of::<R>(),
        });

        // Symmetric set has to happen before exclusivity unset otherwise
        // an entity can get despawned when it shouldn't.
        if R::SYMMETRIC && !self.symmetric_action {
            Command::apply(
                Set::<R> {
                    host: self.target,
                    target: self.host,
                    symmetric_action: true,
                    _phantom: PhantomData,
                },
                world,
            );
        }

        if let Some(old) = old.filter(|old| R::EXCLUSIVE && self.target != *old) {
            Command::apply(UnsetAsymmetric::<R>::new(self.host, old), world);
        }
    }
}

/// Command to remove relationships between entities.
/// This operation is not noisy so if either participant does not exist or
/// the relation does not exist nothing happens.
pub struct Unset<R>
where
    R: Relation,
{
    host: Entity,
    target: Entity,
    _phantom: PhantomData<R>,
}

impl<R: Relation> Unset<R> {
    #[allow(missing_docs)]
    pub fn new(host: Entity, target: Entity) -> Self {
        Self {
            host,
            target,
            _phantom: PhantomData,
        }
    }
}

impl<R: Relation> Command for Unset<R> {
    fn apply(self, world: &mut World) {
        Command::apply(UnsetAsymmetric::<R>::new(self.host, self.target), world);

        if R::SYMMETRIC {
            Command::apply(UnsetAsymmetric::<R>::new(self.target, self.host), world);
        }
    }
}

struct UnsetAsymmetric<R: Relation> {
    host: Entity,
    target: Entity,
    buffered: bool,
    _phantom: PhantomData<R>,
}

impl<R: Relation> UnsetAsymmetric<R> {
    pub fn new(host: Entity, target: Entity) -> Self {
        Self {
            host,
            target,
            buffered: false,
            _phantom: PhantomData,
        }
    }

    pub(crate) fn buffered(host: Entity, target: Entity) -> Self {
        Self {
            host,
            target,
            buffered: true,
            _phantom: PhantomData,
        }
    }
}

impl<R: Relation> Command for UnsetAsymmetric<R> {
    fn apply(self, world: &mut World) {
        let mut host_targets = world
            .get_mut::<Targets<R>>(self.host)
            .map(|mut edges| std::mem::take(&mut *edges))
            .unwrap_or_default();

        let mut target_hosts = world
            .get_mut::<Hosts<R>>(self.target)
            .map(|mut edges| std::mem::take(&mut *edges))
            .unwrap_or_default();

        // Remove edges from containers
        host_targets.remove(self.target);
        target_hosts.remove(self.host);

        let mut host_exists = false;

        if !host_targets.vec.vec.is_empty() {
            world.entity_mut(self.host).insert(host_targets);
            host_exists = true;
        } else if let Some(mut host) = world.get_entity_mut(self.host) {
            host_exists = true;
            host.remove::<Targets<R>>();
        }

        let mut target_exists = false;

        if !target_hosts.vec.vec.is_empty() {
            world.entity_mut(self.target).insert(target_hosts);
            target_exists = true;
        } else if matches!(
            R::CLEANUP_POLICY,
            CleanupPolicy::Counted | CleanupPolicy::Total
        ) {
            if self.buffered {
                world.commands().add(move |world: &mut World| {
                    world.despawn(self.target);
                });
            } else {
                world.despawn(self.target);
            }
        } else if let Some(mut target) = world.get_entity_mut(self.target) {
            target_exists = true;
            target.remove::<Hosts<R>>();
        }

        if host_exists && target_exists {
            world.send_event(TargetEvent {
                host: self.host,
                target_op: Op::Unset,
                target: self.target,
                relation_id: RelationId::of::<R>(),
            });
        }
    }
}

/// Command for entities to untarget all of their relations of a given type.
pub struct UnsetAll<R>
where
    R: Relation,
{
    entity: Entity,
    _phantom: PhantomData<R>,
}

impl<R: Relation> UnsetAll<R> {
    #[allow(missing_docs)]
    pub fn new(entity: Entity) -> Self {
        Self {
            entity,
            _phantom: PhantomData,
        }
    }
}

impl<R: Relation> Command for UnsetAll<R> {
    #[allow(clippy::let_unit_value)]
    fn apply(self, world: &mut World) {
        while let Some(target) = world
            .get::<Targets<R>>(self.entity)
            .and_then(|targets| targets.vec.vec.last())
            .copied()
        {
            let _ = R::ZST_OR_PANIC;

            Command::apply(
                Unset::<R> {
                    target,
                    host: self.entity,
                    _phantom: PhantomData,
                },
                world,
            );
        }
    }
}

/// Command for entities to remove themselves as the target of all relations of a given type.
pub struct Withdraw<R>
where
    R: Relation,
{
    entity: Entity,
    _phantom: PhantomData<R>,
}

impl<R: Relation> Withdraw<R> {
    #[allow(missing_docs)]
    pub fn new(entity: Entity) -> Self {
        Self {
            entity,
            _phantom: PhantomData,
        }
    }
}

impl<R: Relation> Command for Withdraw<R> {
    #[allow(clippy::let_unit_value)]
    fn apply(self, world: &mut World) {
        while let Some(host) = world
            .get::<Hosts<R>>(self.entity)
            .and_then(|hosts| hosts.vec.vec.last())
            .copied()
        {
            let _ = R::ZST_OR_PANIC;

            Command::apply(
                Unset::<R> {
                    host,
                    target: self.entity,
                    _phantom: PhantomData,
                },
                world,
            );
        }
    }
}

/// An extension API to sugar using relation commands.
pub trait RelationCommands {
    /// [`Set`] a relationship target.
    fn set<R: Relation>(&mut self, target: Entity) -> &mut Self;
    /// [`Unset`] a relationship target.
    fn unset<R: Relation>(&mut self, target: Entity) -> &mut Self;
    /// [`UnsetAll`] relationship targets.
    fn unset_all<R: Relation>(&mut self) -> &mut Self;
    /// [`Withdraw`] from a relationship.
    fn withdraw<R: Relation>(&mut self) -> &mut Self;
}

impl RelationCommands for EntityWorldMut<'_> {
    fn set<R: Relation>(&mut self, target: Entity) -> &mut Self {
        let _ = R::ZST_OR_PANIC;

        let id = self.id();
        self.world_scope(|world| {
            Command::apply(Set::<R>::new(id, target), world);
        });

        self.update_location();
        self
    }

    fn unset<R: Relation>(&mut self, target: Entity) -> &mut Self {
        let _ = R::ZST_OR_PANIC;

        let id = self.id();
        self.world_scope(|world| {
            Command::apply(
                Unset::<R> {
                    host: id,
                    target,
                    _phantom: PhantomData,
                },
                world,
            );
        });

        self.update_location();
        self
    }

    fn unset_all<R: Relation>(&mut self) -> &mut Self {
        let _ = R::ZST_OR_PANIC;

        let id = self.id();
        self.world_scope(|world| {
            Command::apply(
                UnsetAll::<R> {
                    entity: id,
                    _phantom: PhantomData,
                },
                world,
            );
        });

        self.update_location();
        self
    }

    fn withdraw<R: Relation>(&mut self) -> &mut Self {
        let _ = R::ZST_OR_PANIC;

        let id = self.id();
        self.world_scope(|world| {
            Command::apply(
                Withdraw::<R> {
                    entity: id,
                    _phantom: PhantomData,
                },
                world,
            );
        });

        self.update_location();
        self
    }
}

impl RelationCommands for EntityCommands<'_> {
    fn set<R: Relation>(&mut self, target: Entity) -> &mut Self {
        let _ = R::ZST_OR_PANIC;

        let id = self.id();
        self.commands().add(Set::<R>::new(id, target));
        self
    }

    fn unset<R: Relation>(&mut self, target: Entity) -> &mut Self {
        let _ = R::ZST_OR_PANIC;

        let id = self.id();
        self.commands().add(Unset::<R> {
            host: id,
            target,
            _phantom: PhantomData,
        });
        self
    }

    fn unset_all<R: Relation>(&mut self) -> &mut Self {
        let _ = R::ZST_OR_PANIC;

        let id = self.id();
        self.commands().add(UnsetAll::<R> {
            entity: id,
            _phantom: PhantomData,
        });
        self
    }

    fn withdraw<R: Relation>(&mut self) -> &mut Self {
        let _ = R::ZST_OR_PANIC;

        let id = self.id();
        self.commands().add(Withdraw::<R> {
            entity: id,
            _phantom: PhantomData,
        });
        self
    }
}

#[cfg(test)]
mod tests {
    use super::Hosts;
    use super::Targets;
    use crate::prelude::*;
    use bevy_ecs::prelude::*;
    use std::array::from_fn;

    fn has_edges<R: Relation>(world: &World, entity: Entity) -> bool {
        world.get::<Hosts<R>>(entity).is_some() || world.get::<Targets<R>>(entity).is_some()
    }

    fn is_root<R: Relation>(world: &World, entity: Entity) -> bool {
        world.get::<Hosts<R>>(entity).is_some() && world.get::<Targets<R>>(entity).is_none()
    }

    fn is_participant<R: Relation>(world: &World, entity: Entity) -> bool {
        world.get::<Hosts<R>>(entity).is_some() || world.get::<Targets<R>>(entity).is_some()
    }

    fn targeting<R: Relation>(world: &World, host: Entity, target: Entity) -> bool {
        let host_is_targeting = world
            .get::<Targets<R>>(host)
            .map_or(false, |vec| vec.vec.vec.contains(&target));

        let target_is_hosted = world
            .get::<Hosts<R>>(target)
            .map_or(false, |vec| vec.vec.vec.contains(&host));

        if host_is_targeting != target_is_hosted {
            panic!("Out of sync edge info");
        }

        host_is_targeting
    }

    #[test]
    fn set_unset() {
        #[derive(Relation)]
        struct R;

        let mut world = World::new();
        let [host, target] = from_fn(|_| world.spawn_empty().id());

        world.entity_mut(host).set::<R>(target);
        assert!(targeting::<R>(&world, host, target));
        assert!(is_participant::<R>(&world, host));
        assert!(is_root::<R>(&world, target));

        world.entity_mut(host).unset::<R>(target);
        assert!(!has_edges::<R>(&world, target));
        assert!(!has_edges::<R>(&world, host));
        assert!(!is_participant::<R>(&world, host));
        assert!(!is_root::<R>(&world, target));
    }

    #[test]
    fn exclusive() {
        #[derive(Relation)]
        struct R;

        let mut world = World::new();
        let [host, t0, t1] = from_fn(|_| world.spawn_empty().id());

        // Before overwrite
        world.entity_mut(host).set::<R>(t0);

        assert!(targeting::<R>(&world, host, t0));
        assert!(is_participant::<R>(&world, host));
        assert!(is_root::<R>(&world, t0));

        // After overwrite
        world.entity_mut(host).set::<R>(t1);

        assert!(targeting::<R>(&world, host, t1));
        assert!(is_participant::<R>(&world, host));
        assert!(is_root::<R>(&world, t1));

        assert!(!has_edges::<R>(&world, t0));
        assert!(!is_root::<R>(&world, t0));
    }

    #[derive(Relation)]
    struct Orphan;

    #[derive(Relation)]
    #[aery(Counted)]
    struct Counted;

    #[derive(Relation)]
    #[aery(Recursive)]
    struct Recursive;

    #[derive(Relation)]
    #[aery(Total)]
    struct Total;

    #[derive(Debug)]
    struct TestEdges {
        orphan: Entity,
        counted: Entity,
        recursive: Entity,
        total: Entity,
    }

    #[derive(Debug)]
    struct Test {
        center: Entity,
        targets: TestEdges,
        hosts: TestEdges,
    }

    impl Test {
        fn new(world: &mut World) -> Self {
            let test = Self {
                center: world.spawn_empty().id(),
                targets: TestEdges {
                    orphan: world.spawn_empty().id(),
                    counted: world.spawn_empty().id(),
                    recursive: world.spawn_empty().id(),
                    total: world.spawn_empty().id(),
                },
                hosts: TestEdges {
                    orphan: world.spawn_empty().id(),
                    counted: world.spawn_empty().id(),
                    recursive: world.spawn_empty().id(),
                    total: world.spawn_empty().id(),
                },
            };

            world
                .entity_mut(test.hosts.orphan)
                .set::<Orphan>(test.center);
            world
                .entity_mut(test.center)
                .set::<Orphan>(test.targets.orphan);

            world
                .entity_mut(test.hosts.counted)
                .set::<Counted>(test.center);
            world
                .entity_mut(test.center)
                .set::<Counted>(test.targets.counted);

            world
                .entity_mut(test.hosts.recursive)
                .set::<Recursive>(test.center);
            world
                .entity_mut(test.center)
                .set::<Recursive>(test.targets.recursive);

            world.entity_mut(test.hosts.total).set::<Total>(test.center);
            world
                .entity_mut(test.center)
                .set::<Total>(test.targets.total);

            test
        }

        fn assert_unchanged(&self, world: &World) {
            assert!(targeting::<Orphan>(world, self.hosts.orphan, self.center));
            assert!(targeting::<Orphan>(world, self.center, self.targets.orphan));
            assert!(is_participant::<Orphan>(world, self.hosts.orphan,));
            assert!(is_root::<Orphan>(world, self.targets.orphan));

            assert!(targeting::<Counted>(world, self.hosts.counted, self.center));
            assert!(targeting::<Counted>(
                world,
                self.center,
                self.targets.counted
            ));
            assert!(is_participant::<Counted>(world, self.hosts.counted,));
            assert!(is_root::<Counted>(world, self.targets.counted));

            assert!(targeting::<Recursive>(
                world,
                self.hosts.recursive,
                self.center
            ));
            assert!(targeting::<Recursive>(
                world,
                self.center,
                self.targets.recursive
            ));
            assert!(is_participant::<Recursive>(world, self.hosts.recursive,));
            assert!(is_root::<Recursive>(world, self.targets.recursive));

            assert!(targeting::<Total>(world, self.hosts.total, self.center));
            assert!(targeting::<Total>(world, self.center, self.targets.total));
            assert!(is_participant::<Total>(world, self.hosts.total));
            assert!(is_root::<Total>(world, self.targets.total));

            assert!(is_participant::<Orphan>(world, self.center));
            assert!(is_participant::<Counted>(world, self.center));
            assert!(is_participant::<Recursive>(world, self.center));
            assert!(is_participant::<Total>(world, self.center));
        }

        fn assert_cleaned(&self, world: &World) {
            assert!(world.get_entity(self.center).is_none());

            assert!(
                !(has_edges::<Orphan>(world, self.hosts.orphan)
                    || has_edges::<Counted>(world, self.hosts.orphan)
                    || has_edges::<Recursive>(world, self.hosts.orphan)
                    || has_edges::<Total>(world, self.hosts.orphan))
            );
            assert!(
                !(has_edges::<Orphan>(world, self.targets.orphan)
                    || has_edges::<Counted>(world, self.targets.orphan)
                    || has_edges::<Recursive>(world, self.targets.orphan)
                    || has_edges::<Total>(world, self.targets.orphan))
            );
            assert!(!is_participant::<Orphan>(world, self.hosts.orphan));
            assert!(!is_root::<Orphan>(world, self.targets.orphan));

            assert!(world.get_entity(self.targets.counted).is_none());
            assert!(
                !(has_edges::<Orphan>(world, self.hosts.counted)
                    || has_edges::<Counted>(world, self.hosts.counted)
                    || has_edges::<Recursive>(world, self.hosts.counted)
                    || has_edges::<Total>(world, self.hosts.counted))
            );
            assert!(!is_participant::<Counted>(world, self.hosts.counted,));

            assert!(world.get_entity(self.hosts.recursive).is_none());
            assert!(
                !(has_edges::<Orphan>(world, self.targets.recursive)
                    || has_edges::<Counted>(world, self.targets.recursive)
                    || has_edges::<Recursive>(world, self.targets.recursive)
                    || has_edges::<Total>(world, self.targets.recursive))
            );
            assert!(!is_root::<Recursive>(world, self.targets.recursive));

            assert!(world.get_entity(self.hosts.total).is_none());
            assert!(world.get_entity(self.targets.total).is_none());
        }
    }

    #[test]
    fn orphan_in_despawned() {
        #[derive(Relation)]
        struct R;

        let mut world = World::new();

        let test = Test::new(&mut world);

        let mut e = world.spawn_empty();
        e.set::<R>(test.center);
        e.despawn();

        test.assert_unchanged(&world);
        assert!(!is_participant::<R>(&world, test.center));
    }

    #[test]
    fn orphan_out_despawned() {
        #[derive(Relation)]
        struct R;

        let mut world = World::new();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.entity_mut(test.center).set::<R>(e);
        world.entity_mut(e).despawn();

        test.assert_unchanged(&world);
        assert!(!is_participant::<R>(&world, test.center));
    }

    #[test]
    fn counted_in_despawned() {
        #[derive(Relation)]
        #[aery(Counted)]
        struct R;

        let mut world = World::new();

        let test = Test::new(&mut world);

        let mut e = world.spawn_empty();
        e.set::<R>(test.center);
        e.despawn();

        test.assert_cleaned(&world);
    }

    #[test]
    fn counted_out_despawned() {
        #[derive(Relation)]
        #[aery(Counted)]
        struct R;

        let mut world = World::new();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.entity_mut(test.center).set::<R>(e);
        world.entity_mut(e).despawn();

        test.assert_unchanged(&world);
        assert!(!is_participant::<R>(&world, test.center));
    }

    #[test]
    fn recursive_in_despawned() {
        #[derive(Relation)]
        #[aery(Recursive)]
        struct R;

        let mut world = World::new();

        let test = Test::new(&mut world);

        let mut e = world.spawn_empty();
        e.set::<R>(test.center);
        e.despawn();

        test.assert_unchanged(&world);
        assert!(!is_participant::<R>(&world, test.center));
    }

    #[test]
    fn recursive_out_despawned() {
        #[derive(Relation)]
        #[aery(Recursive)]
        struct R;

        let mut world = World::new();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.entity_mut(test.center).set::<R>(e);
        world.entity_mut(e).despawn();

        test.assert_cleaned(&world);
    }

    #[test]
    fn total_in_despawned() {
        #[derive(Relation)]
        #[aery(Total)]
        struct R;

        let mut world = World::new();

        let test = Test::new(&mut world);

        let mut e = world.spawn_empty();
        e.set::<R>(test.center);
        e.despawn();

        test.assert_cleaned(&world);
    }

    #[test]
    fn total_out_despawned() {
        #[derive(Relation)]
        #[aery(Total)]
        struct R;

        let mut world = World::new();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.entity_mut(test.center).set::<R>(e);
        world.entity_mut(e).despawn();

        test.assert_cleaned(&world);
    }

    #[test]
    fn orphan_in_unset() {
        #[derive(Relation)]
        struct R;

        let mut world = World::new();

        let test = Test::new(&mut world);

        world
            .spawn_empty()
            .set::<R>(test.center)
            .unset::<R>(test.center);

        test.assert_unchanged(&world);
        assert!(!is_participant::<R>(&world, test.center));
    }

    #[test]
    fn orphan_out_unset() {
        #[derive(Relation)]
        struct R;

        let mut world = World::new();

        let test = Test::new(&mut world);
        let e = world.spawn_empty().id();
        world.entity_mut(test.center).set::<R>(e).unset::<R>(e);

        test.assert_unchanged(&world);
        assert!(!is_participant::<R>(&world, test.center));
    }

    #[test]
    fn counted_in_unset() {
        #[derive(Relation)]
        #[aery(Counted)]
        struct R;

        let mut world = World::new();

        let test = Test::new(&mut world);

        world
            .spawn_empty()
            .set::<R>(test.center)
            .unset::<R>(test.center);

        test.assert_cleaned(&world);
    }

    #[test]
    fn counted_out_unset() {
        #[derive(Relation)]
        #[aery(Counted)]
        struct R;

        let mut world = World::new();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();

        world.entity_mut(test.center).set::<R>(e).unset::<R>(e);

        test.assert_unchanged(&world);
        assert!(!is_participant::<R>(&world, test.center));
    }

    #[test]
    fn recursive_in_unset() {
        #[derive(Relation)]
        #[aery(Recursive)]
        struct R;

        let mut world = World::new();

        let test = Test::new(&mut world);

        world
            .spawn_empty()
            .set::<R>(test.center)
            .unset::<R>(test.center);

        test.assert_unchanged(&world);
        assert!(!is_participant::<R>(&world, test.center));
    }

    #[test]
    #[should_panic]
    fn recursive_out_unset() {
        #[derive(Relation)]
        #[aery(Recursive)]
        struct R;

        let mut world = World::new();

        let test = Test::new(&mut world);
        let e = world.spawn_empty().id();
        world.entity_mut(test.center).set::<R>(e).unset::<R>(e);

        test.assert_cleaned(&world);
    }

    #[test]
    fn total_in_unset() {
        #[derive(Relation)]
        #[aery(Total)]
        struct R;

        let mut world = World::new();

        let test = Test::new(&mut world);

        world
            .spawn_empty()
            .set::<R>(test.center)
            .unset::<R>(test.center);

        test.assert_cleaned(&world);
    }

    #[test]
    #[should_panic]
    fn total_out_unset() {
        #[derive(Relation)]
        #[aery(Total)]
        struct R;

        let mut world = World::new();

        let test = Test::new(&mut world);
        let e = world.spawn_empty().id();
        world.entity_mut(test.center).set::<R>(e).unset::<R>(e);

        test.assert_cleaned(&world);
    }
}
