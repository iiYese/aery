use std::marker::PhantomData;

use smallvec::SmallVec;

use bevy_derive::{Deref, DerefMut};
use bevy_ecs::{
    component::{Component, ComponentHooks, ComponentId, StorageType},
    entity::{Entity, EntityMapper, MapEntities},
    event::Event,
    query::{AnyOf, Changed, Or, QueryData, QueryFilter, With, Without},
    reflect::{ReflectComponent, ReflectMapEntities},
    system::EntityCommands,
    world::{Command, DeferredWorld, EntityWorldMut, World},
};
use bevy_hierarchy::{Children, Parent};
use bevy_log::warn;
use bevy_reflect::{utility::GenericTypePathCell, Reflect, TypePath};

use crate::relation::{CleanupPolicy, Relation, ZstOrPanic};

// Small Stable Unique Vec
#[derive(Reflect)]
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

    pub(crate) fn remove(&mut self, val: T) -> bool {
        if let Some(n) = self
            .vec
            .iter()
            .enumerate()
            .find_map(|(n, item)| (*item == val).then_some(n))
        {
            self.vec.remove(n);
            true
        } else {
            false
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
        cmds.queue(UnsetAsymmetric::<R>::buffered(host, id));
    }

    for target in targets.iter().copied() {
        cmds.queue(UnsetAsymmetric::<R>::buffered(id, target));
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
        cmds.queue(move |world: &mut World| {
            world.despawn(host);
        });
    }

    for target in targets.iter().copied() {
        cmds.queue(UnsetAsymmetric::<R>::buffered(id, target));
    }
}

#[derive(Deref, DerefMut, Reflect)]
#[reflect(Component, MapEntities, type_path = false, where R: Relation)]
pub(crate) struct Hosts<R: Relation> {
    #[deref]
    pub(crate) vec: SSUVec<Entity>,
    #[reflect(ignore)]
    _phantom: PhantomData<R>,
}

impl<R: Relation> MapEntities for Hosts<R> {
    fn map_entities<M: EntityMapper>(&mut self, entity_mapper: &mut M) {
        for entity in self.vec.vec.iter_mut() {
            *entity = entity_mapper.map_entity(*entity);
        }
    }
}

impl<R: Relation> TypePath for Hosts<R> {
    fn type_path() -> &'static str {
        static CELL: GenericTypePathCell = GenericTypePathCell::new();
        CELL.get_or_insert::<Self, _>(|| {
            format!("aery::edges::Hosts<{}>", std::any::type_name::<R>())
        })
    }

    fn short_type_path() -> &'static str {
        static CELL: GenericTypePathCell = GenericTypePathCell::new();
        CELL.get_or_insert::<Self, _>(|| format!("Hosts<{}>", std::any::type_name::<R>()))
    }

    fn type_ident() -> Option<&'static str> {
        Some(std::any::type_name::<R>())
    }

    fn crate_name() -> Option<&'static str> {
        Some("aery")
    }
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

#[derive(Deref, DerefMut, Reflect)]
#[reflect(Component, MapEntities, type_path = false, where R: Relation)]
pub(crate) struct Targets<R: Relation> {
    #[deref]
    pub(crate) vec: SSUVec<Entity>,
    #[reflect(ignore)]
    _phantom: PhantomData<R>,
}

impl<R: Relation> MapEntities for Targets<R> {
    fn map_entities<M: EntityMapper>(&mut self, entity_mapper: &mut M) {
        for entity in self.vec.vec.iter_mut() {
            *entity = entity_mapper.map_entity(*entity);
        }
    }
}

impl<R: Relation> TypePath for Targets<R> {
    fn type_path() -> &'static str {
        static CELL: GenericTypePathCell = GenericTypePathCell::new();
        CELL.get_or_insert::<Self, _>(|| {
            format!("aery::edges::Targets<{}>", std::any::type_name::<R>())
        })
    }

    fn short_type_path() -> &'static str {
        static CELL: GenericTypePathCell = GenericTypePathCell::new();
        CELL.get_or_insert::<Self, _>(|| format!("Targets<{}>", std::any::type_name::<R>()))
    }

    fn type_ident() -> Option<&'static str> {
        Some(std::any::type_name::<R>())
    }

    fn crate_name() -> Option<&'static str> {
        Some("aery")
    }
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

/// Event triggered whenever an entity gets a new relation
#[derive(Event, Clone, Copy, Debug)]
pub struct SetEvent<R: Relation> {
    /// The target entity of the event. The triggers entity is the host.
    pub target: Entity,
    _phantom: PhantomData<R>,
}

/// Event triggered whenever an entity loses a relation
#[derive(Event, Clone, Copy, Debug)]
pub struct UnsetEvent<R: Relation> {
    /// The target entity of the event. The triggers entity is the host.
    pub target: Entity,
    _phantom: PhantomData<R>,
}

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

        if world.get_entity(self.target).is_err() {
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

        if world.get_entity(self.host).is_err() {
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

        let mut old: Option<Entity> = None;

        let mut host_entity = world.entity_mut(self.host);
        if let Some(mut host_targets) = host_entity.get_mut::<Targets<R>>() {
            // Check if this target is already present
            if host_targets.vec.vec.contains(&self.target) {
                return;
            } else {
                // Take the current Targets<R> value, modify it, and then reinsert it
                let mut new_host_targets = std::mem::take(&mut *host_targets);
                old = new_host_targets.vec.vec.first().copied();
                new_host_targets.add(self.target);
                *host_targets = new_host_targets;
            }
        } else {
            // If Targets<R> doesn't exist on the host, create and insert a new one
            let mut new_host_targets = Targets::<R>::default();
            new_host_targets.add(self.target);
            host_entity.insert(new_host_targets);
        }

        let mut target_entity = world.entity_mut(self.target);
        if let Some(mut target_hosts) = target_entity.get_mut::<Hosts<R>>() {
            // Check if this host is already present
            if target_hosts.vec.vec.contains(&self.host) {
                return;
            } else {
                // Take the current Hosts<R> value, modify it, and then reinsert it
                let mut new_target_hosts = std::mem::take(&mut *target_hosts);
                new_target_hosts.vec.add(self.host);
                *target_hosts = new_target_hosts;
            }
        } else {
            // If Hosts<R> doesn't exist on the target, create and insert a new one
            let mut new_target_hosts = Hosts::<R>::default();
            new_target_hosts.vec.add(self.host);
            target_entity.insert(new_target_hosts);
        }

        world.trigger_targets(
            SetEvent::<R> {
                target: self.target,
                _phantom: PhantomData,
            },
            self.host,
        );

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

        let target_has_host_as_target = if let Some(targets) = world.get::<Targets<R>>(self.target)
        {
            targets.vec.vec.contains(&self.host)
        } else {
            false
        };

        // Remove edges from containers
        let target_removed_from_host = host_targets.remove(self.target);

        let host_removed_from_target = target_hosts.remove(self.host);

        let mut host_entity_exists_in_world = false;

        if !host_targets.vec.vec.is_empty() {
            world.entity_mut(self.host).insert(host_targets);
            host_entity_exists_in_world = true;
        } else if let Ok(mut host) = world.get_entity_mut(self.host) {
            host_entity_exists_in_world = true;
            host.remove::<Targets<R>>();
        }

        let mut target_entity_exists_in_world = false;

        if !target_hosts.vec.vec.is_empty() {
            world.entity_mut(self.target).insert(target_hosts);
            target_entity_exists_in_world = true;
        } else if matches!(
            R::CLEANUP_POLICY,
            CleanupPolicy::Counted | CleanupPolicy::Total
        ) {
            if self.buffered {
                world.commands().queue(move |world: &mut World| {
                    world.despawn(self.target);
                });
            } else {
                world.despawn(self.target);
            }
        } else if let Ok(mut target) = world.get_entity_mut(self.target) {
            target_entity_exists_in_world = true;
            target.remove::<Hosts<R>>();
        }

        if target_removed_from_host && host_entity_exists_in_world {
            world.trigger_targets(
                UnsetEvent::<R> {
                    target: self.target,
                    _phantom: PhantomData,
                },
                self.host,
            );
        }
        // Need to check situation if !R::EXCLUSIVE - it is possible that the target also has us as target, so we need to send event
        // to the target also
        if host_removed_from_target && target_entity_exists_in_world {
            if (R::SYMMETRIC && R::EXCLUSIVE) || (!R::EXCLUSIVE && target_has_host_as_target) {
                world.trigger_targets(
                    UnsetEvent::<R> {
                        target: self.host,
                        _phantom: PhantomData,
                    },
                    self.target,
                );
            }
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
        self.commands().queue(Set::<R>::new(id, target));
        self
    }

    fn unset<R: Relation>(&mut self, target: Entity) -> &mut Self {
        let _ = R::ZST_OR_PANIC;

        let id = self.id();
        self.commands().queue(Unset::<R> {
            host: id,
            target,
            _phantom: PhantomData,
        });
        self
    }

    fn unset_all<R: Relation>(&mut self) -> &mut Self {
        let _ = R::ZST_OR_PANIC;

        let id = self.id();
        self.commands().queue(UnsetAll::<R> {
            entity: id,
            _phantom: PhantomData,
        });
        self
    }

    fn withdraw<R: Relation>(&mut self) -> &mut Self {
        let _ = R::ZST_OR_PANIC;

        let id = self.id();
        self.commands().queue(Withdraw::<R> {
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
    use bevy::prelude::*;
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
            assert!(world.get_entity(self.center).is_err());

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

            assert!(world.get_entity(self.targets.counted).is_err());
            assert!(
                !(has_edges::<Orphan>(world, self.hosts.counted)
                    || has_edges::<Counted>(world, self.hosts.counted)
                    || has_edges::<Recursive>(world, self.hosts.counted)
                    || has_edges::<Total>(world, self.hosts.counted))
            );
            assert!(!is_participant::<Counted>(world, self.hosts.counted,));

            assert!(world.get_entity(self.hosts.recursive).is_err());
            assert!(
                !(has_edges::<Orphan>(world, self.targets.recursive)
                    || has_edges::<Counted>(world, self.targets.recursive)
                    || has_edges::<Recursive>(world, self.targets.recursive)
                    || has_edges::<Total>(world, self.targets.recursive))
            );
            assert!(!is_root::<Recursive>(world, self.targets.recursive));

            assert!(world.get_entity(self.hosts.total).is_err());
            assert!(world.get_entity(self.targets.total).is_err());
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
    #[derive(Component)]
    struct EventCounter(i32);

    #[derive(Relation)]
    struct AsymmetricRelation;

    #[derive(Relation)]
    #[aery(Symmetric)]
    struct SymmetricRelation;

    #[derive(Relation)]
    #[aery(Poly)]
    struct PolyRelation;

    #[derive(Relation)]
    #[aery(Poly, Symmetric)]
    struct PolySymmetricRelation;

    enum RelationType {
        Asymmetric,
        Symmetric,
        Poly,
        PolySymmetric,
    }

    fn test_unset_event<R: Relation>(
        world: &mut World,
        relation_name: &str,
        set_both_ways: bool,
        expected_counter_a: i32,
        expected_counter_b: i32,
    ) {
        // Spawn two entities with EventCounter and observe UnsetEvent<R>
        let entity_a = world
            .spawn(EventCounter(0))
            .observe(
                |trigger: Trigger<UnsetEvent<R>>, mut counters: Query<&mut EventCounter>| {
                    counters.get_mut(trigger.entity()).unwrap().0 += 1;
                },
            )
            .id();

        let entity_b = world
            .spawn(EventCounter(0))
            .observe(
                |trigger: Trigger<UnsetEvent<R>>, mut counters: Query<&mut EventCounter>| {
                    counters.get_mut(trigger.entity()).unwrap().0 += 1;
                },
            )
            .id();

        world.flush();

        // Set the relation(s)
        world.entity_mut(entity_a).set::<R>(entity_b);
        if set_both_ways {
            world.entity_mut(entity_b).set::<R>(entity_a);
        }

        world.flush();

        // Unset the relation from entity_a to entity_b
        world.entity_mut(entity_a).unset::<R>(entity_b);
        world.flush();

        // Check counters
        let counter_a = world.get::<EventCounter>(entity_a).unwrap().0;
        let counter_b = world.get::<EventCounter>(entity_b).unwrap().0;

        assert_eq!(
            counter_a, expected_counter_a,
            "{}: Unexpected counter value for entity_a",
            relation_name
        );
        assert_eq!(
            counter_b, expected_counter_b,
            "{}: Unexpected counter value for entity_b",
            relation_name
        );
    }

    #[test]
    fn test_unset_events_for_relations() {
        let mut world = World::new();

        // Register relations
        world.register_relation::<AsymmetricRelation>();
        world.register_relation::<SymmetricRelation>();
        world.register_relation::<PolyRelation>();
        world.register_relation::<PolySymmetricRelation>();

        // Test cases: (RelationType, set_both_ways, expected counters after unset)
        let test_cases = vec![
            (RelationType::Asymmetric, false, 1, 0),
            (RelationType::Symmetric, false, 1, 1),
            (RelationType::Poly, true, 1, 1),
            (RelationType::PolySymmetric, true, 1, 1),
        ];

        for (relation_type, set_both_ways, expected_a, expected_b) in test_cases {
            match relation_type {
                RelationType::Asymmetric => test_unset_event::<AsymmetricRelation>(
                    &mut world,
                    "AsymmetricRelation",
                    set_both_ways,
                    expected_a,
                    expected_b,
                ),
                RelationType::Symmetric => test_unset_event::<SymmetricRelation>(
                    &mut world,
                    "SymmetricRelation",
                    set_both_ways,
                    expected_a,
                    expected_b,
                ),
                RelationType::Poly => test_unset_event::<PolyRelation>(
                    &mut world,
                    "PolyRelation",
                    set_both_ways,
                    expected_a,
                    expected_b,
                ),
                RelationType::PolySymmetric => test_unset_event::<PolySymmetricRelation>(
                    &mut world,
                    "PolySymmetricRelation",
                    set_both_ways,
                    expected_a,
                    expected_b,
                ),
            }
        }
    }
    fn test_set_event<R: Relation>(
        world: &mut World,
        relation_name: &str,
        set_both_ways: bool,
        expected_counter_a: i32,
        expected_counter_b: i32,
    ) {
        // Spawn two entities with EventCounter and observe SetEvent<R>
        let entity_a = world
            .spawn(EventCounter(0))
            .observe(
                |trigger: Trigger<SetEvent<R>>, mut counters: Query<&mut EventCounter>| {
                    counters.get_mut(trigger.entity()).unwrap().0 += 1;
                },
            )
            .id();

        let entity_b = world
            .spawn(EventCounter(0))
            .observe(
                |trigger: Trigger<SetEvent<R>>, mut counters: Query<&mut EventCounter>| {
                    counters.get_mut(trigger.entity()).unwrap().0 += 1;
                },
            )
            .id();

        world.flush();

        // Set the relation(s)
        world.entity_mut(entity_a).set::<R>(entity_b);
        if set_both_ways {
            world.entity_mut(entity_b).set::<R>(entity_a);
        }

        world.flush();

        // Check counters
        let counter_a = world.get::<EventCounter>(entity_a).unwrap().0;
        let counter_b = world.get::<EventCounter>(entity_b).unwrap().0;

        assert_eq!(
            counter_a, expected_counter_a,
            "{}: Unexpected counter value for entity_a",
            relation_name
        );
        assert_eq!(
            counter_b, expected_counter_b,
            "{}: Unexpected counter value for entity_b",
            relation_name
        );
    }

    #[test]
    fn test_set_events_for_relations() {
        // Create a new world for each test case to avoid state carry-over
        let mut world = World::new();

        // Register relations
        world.register_relation::<AsymmetricRelation>();
        world.register_relation::<SymmetricRelation>();
        world.register_relation::<PolyRelation>();
        world.register_relation::<PolySymmetricRelation>();

        // Test cases: (RelationType, set_both_ways, expected_counter_a, expected_counter_b)
        let test_cases = vec![
            (RelationType::Asymmetric, false, 1, 0),
            (RelationType::Symmetric, false, 1, 1),
            (RelationType::Poly, true, 1, 1),
            (RelationType::PolySymmetric, true, 1, 1),
        ];

        for (relation_type, set_both_ways, expected_a, expected_b) in test_cases {
            match relation_type {
                RelationType::Asymmetric => test_set_event::<AsymmetricRelation>(
                    &mut world,
                    "AsymmetricRelation",
                    set_both_ways,
                    expected_a,
                    expected_b,
                ),
                RelationType::Symmetric => test_set_event::<SymmetricRelation>(
                    &mut world,
                    "SymmetricRelation",
                    set_both_ways,
                    expected_a,
                    expected_b,
                ),
                RelationType::Poly => test_set_event::<PolyRelation>(
                    &mut world,
                    "PolyRelation",
                    set_both_ways,
                    expected_a,
                    expected_b,
                ),
                RelationType::PolySymmetric => test_set_event::<PolySymmetricRelation>(
                    &mut world,
                    "PolySymmetricRelation",
                    set_both_ways,
                    expected_a,
                    expected_b,
                ),
            }
        }
    }
}
