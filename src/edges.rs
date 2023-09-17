use crate::relation::{CleanupPolicy, Relation, ZstOrPanic};

use bevy::{
    ecs::{
        component::Component,
        entity::Entity,
        query::{Or, With, Without, WorldQuery},
        system::{Command, CommandQueue},
        world::{EntityMut, World},
    },
    hierarchy::{Children, Parent},
    log::warn,
    prelude::{Deref, DerefMut},
};

use smallvec::SmallVec;
use std::{collections::VecDeque, marker::PhantomData};

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
    pub fn add(&mut self, val: T) {
        if self.vec.iter().all(|item| *item != val) {
            self.vec.push(val)
        }
    }

    pub fn remove(&mut self, val: T) {
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

#[derive(Component, Deref, DerefMut)]
pub(crate) struct Hosts<R: Relation> {
    #[deref]
    vec: SSUVec<Entity>,
    _phantom: PhantomData<R>,
}

#[derive(Component, Deref, DerefMut)]
pub(crate) struct Targets<R: Relation> {
    #[deref]
    vec: SSUVec<Entity>,
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

impl<R: Relation> Default for Targets<R> {
    fn default() -> Self {
        Self {
            vec: SSUVec::default(),
            _phantom: PhantomData,
        }
    }
}

#[derive(Component, Default, Deref, DerefMut)]
pub(crate) struct OnDelete {
    #[deref]
    pub hooks: SSUVec<fn(Entity, &mut World, &mut VecDeque<Entity>)>,
}

#[derive(WorldQuery)]
pub struct Edges<R: Relation> {
    pub(crate) hosts: Option<&'static Hosts<R>>,
    pub(crate) targets: Option<&'static Targets<R>>,
    pub(crate) _filter: Or<(With<Hosts<R>>, With<Targets<R>>)>,
}

#[derive(WorldQuery)]
pub struct HierarchyEdges {
    pub(crate) parent: Option<&'static Parent>,
    pub(crate) children: Option<&'static Children>,
    pub(crate) _filter: Or<(With<Parent>, With<Children>)>,
}

/// Filter to find roots of a relationship graph for quintessential traversal.
/// A root of any `R` is an entity that is the target of atleast 1 `R`
/// but does not itself target any other entities with `R`.
#[derive(WorldQuery)]
pub struct Root<R: Relation>((With<Hosts<R>>, Without<Targets<R>>));

#[derive(WorldQuery)]
pub struct Branch<R: Relation>((With<Hosts<R>>, With<Targets<R>>));

#[derive(WorldQuery)]
pub struct Leaf<R: Relation>((Without<Hosts<R>>, With<Targets<R>>));

#[derive(WorldQuery)]
pub struct Participates<R: Relation>(Or<(With<Hosts<R>>, With<Targets<R>>)>);

#[derive(WorldQuery)]
pub struct Abstains<R: Relation>((Without<Hosts<R>>, Without<Targets<R>>));

// Cleanup functions go in both directions to prevent cleanup depending on if a host was
// added/removed first or if a target was added/removed first.

// For cleanup types:
// - Orphan
// - Counted
pub(crate) fn unset_edges<R: Relation>(id: Entity, world: &mut World, _: &mut VecDeque<Entity>) {
    let targets = world
        .get_mut::<Targets<R>>(id)
        .map(|mut targets| std::mem::take(&mut *targets))
        .unwrap_or_default();

    for target in targets.vec.vec.iter().copied() {
        Command::apply(UnsetAsymmetric::<R>::new(id, target), world);
    }

    let hosts = world
        .get_mut::<Hosts<R>>(id)
        .map(|mut hosts| std::mem::take(&mut *hosts))
        .unwrap_or_default();

    for host in hosts.vec.vec.iter().copied() {
        Command::apply(UnsetAsymmetric::<R>::new(host, id), world);
    }
}

// For cleanup types:
// - Recrusive
// - Total
pub(crate) fn clean_recursive<R: Relation>(
    id: Entity,
    world: &mut World,
    queue: &mut VecDeque<Entity>,
) {
    let targets = world
        .get_mut::<Targets<R>>(id)
        .map(|mut edges| std::mem::take(&mut *edges))
        .unwrap_or_default();

    for target in targets.vec.vec.iter().copied() {
        Command::apply(UnsetAsymmetric::<R>::new(id, target), world);
    }

    if let Some(hosts) = world
        .get_mut::<Hosts<R>>(id)
        .map(|mut edges| std::mem::take(&mut *edges).vec.vec)
    {
        queue.extend(hosts);
    }
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

        // Add cleanup
        let mut host_cleanup = world
            .entity_mut(self.host)
            .get_mut::<OnDelete>()
            .map(|mut cleanup| std::mem::take(&mut *cleanup))
            .unwrap_or_default();

        let mut target_cleanup = world
            .entity_mut(self.target)
            .get_mut::<OnDelete>()
            .map(|mut cleanup| std::mem::take(&mut *cleanup))
            .unwrap_or_default();

        match R::CLEANUP_POLICY {
            CleanupPolicy::Orphan | CleanupPolicy::Counted => {
                host_cleanup.add(unset_edges::<R>);
                target_cleanup.add(unset_edges::<R>);
            }
            _ => {
                host_cleanup.add(clean_recursive::<R>);
                target_cleanup.add(clean_recursive::<R>);
            }
        }

        // add target
        let mut host_targets = world
            .entity_mut(self.host)
            .get_mut::<Targets<R>>()
            .map(|mut targets| std::mem::take(&mut *targets))
            .unwrap_or_default();

        let old = host_targets.vec.vec.first().copied();
        host_targets.add(self.target);

        world
            .entity_mut(self.host)
            .insert(host_targets)
            .insert(host_cleanup);

        // add host
        let mut target_hosts = world
            .entity_mut(self.target)
            .get_mut::<Hosts<R>>()
            .map(|mut hosts| std::mem::take(&mut *hosts))
            .unwrap_or_default();

        target_hosts.vec.add(self.host);

        world
            .entity_mut(self.target)
            .insert(target_hosts)
            .insert(target_cleanup);

        // TODO: Events

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
            Command::apply(
                UnsetAsymmetric::<R> {
                    host: self.host,
                    target: old,
                    _phantom: PhantomData,
                },
                world,
            );
        }
    }
}

/// Command to remove relationships between entities
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
    _phantom: PhantomData<R>,
}

impl<R: Relation> UnsetAsymmetric<R> {
    pub fn new(host: Entity, target: Entity) -> Self {
        Self {
            host,
            target,
            _phantom: PhantomData,
        }
    }
}

impl<R: Relation> Command for UnsetAsymmetric<R> {
    fn apply(self, world: &mut World) {
        let Some(mut host_targets) = world
            .get_mut::<Targets<R>>(self.host)
            .map(|mut edges| std::mem::take(&mut *edges))
        else {
            return;
        };

        let Some(mut target_hosts) = world
            .get_mut::<Hosts<R>>(self.target)
            .map(|mut edges| std::mem::take(&mut *edges))
        else {
            world.entity_mut(self.host).insert(host_targets);
            return;
        };

        // Remove edges from containers
        host_targets.remove(self.target);
        target_hosts.remove(self.host);

        // Check & resinsert containers
        if !host_targets.vec.vec.is_empty() {
            world.entity_mut(self.host).insert(host_targets);
        } else {
            let mut host = world.entity_mut(self.host);
            host.remove::<Targets<R>>();

            let mut cleanup = host
                .get_mut::<OnDelete>()
                .map(|mut cleanup| std::mem::take(&mut *cleanup))
                .unwrap_or_default();

            if host.get::<Targets<R>>().is_none() {
                cleanup.remove(match R::CLEANUP_POLICY {
                    CleanupPolicy::Orphan | CleanupPolicy::Counted => unset_edges::<R>,
                    CleanupPolicy::Recursive | CleanupPolicy::Total => clean_recursive::<R>,
                })
            }

            if cleanup.vec.is_empty() {
                host.remove::<OnDelete>();
            } else {
                host.insert(cleanup);
            }
        }

        if !target_hosts.vec.vec.is_empty() {
            world.entity_mut(self.target).insert(target_hosts);
        } else if matches!(
            R::CLEANUP_POLICY,
            CleanupPolicy::Counted | CleanupPolicy::Total
        ) {
            Command::apply(CheckedDespawn(self.target), world);
        } else {
            let mut target = world.entity_mut(self.target);
            target.remove::<Hosts<R>>();

            let mut cleanup = target
                .get_mut::<OnDelete>()
                .map(|mut cleanup| std::mem::take(&mut *cleanup))
                .unwrap_or_default();

            if target.get::<Targets<R>>().is_none() {
                cleanup.remove(match R::CLEANUP_POLICY {
                    CleanupPolicy::Orphan => unset_edges::<R>,
                    CleanupPolicy::Recursive => clean_recursive::<R>,
                    _ => unreachable!(),
                })
            }

            if cleanup.vec.is_empty() {
                target.remove::<OnDelete>();
            } else {
                target.insert(cleanup);
            }
        }

        // TODO: Events
    }
}

/// Command to despawn entities with rleations. Despawning via any other method can lead to
/// dangling which will not produce correct behavior!
pub struct CheckedDespawn(pub Entity);

impl Command for CheckedDespawn {
    fn apply(self, world: &mut World) {}
}

/// An extension API for `EntityMut<'_>` to sugar using relation commands.
/// Since changing relations can trigger cleanup procedures that might despawn the `Entity` referred
/// to by `EntytMut<'_>` each method is consuming and returns an `Option<EntityMut<'_>>`.
///
/// For convenience this trait is also implemented for `Option<EntityMut<'_>>`. Where the methods
/// are essentially their non-option equivalent wrapped in an implicit [`Option::and_then`] call.
/// `Option::<EntityMut<'_>>::set` will emit a warning if called on an option that is `None`.
/// All of the other functions will not emit a warning as unsetting relations that don't exist and
/// despawning entities that don't exist are not considered an error.
///
/// See [`Scope`] for extension APIs that can operate on relation participants including spawning
/// them.
pub trait RelationCommands<'a>: Sized {
    fn set<R: Relation>(self, target: Entity) -> Option<EntityMut<'a>>;
    fn unset<R: Relation>(self, target: Entity) -> Option<EntityMut<'a>>;
    fn checked_despawn(self);
}

/*#[cfg(test)]
mod tests {
    use super::*;
    use crate::{self as aery, prelude::*};
    use std::array::from_fn;

    fn has_edges(world: &World, entity: Entity) -> bool {
        world.get::<Edges>(entity).is_some()
    }

    fn is_root<R: Relation>(world: &World, entity: Entity) -> bool {
        world.get::<RootMarker<R>>(entity).is_some()
    }

    fn is_participant<R: Relation>(world: &World, entity: Entity) -> bool {
        world.get::<Participant<R>>(entity).is_some()
    }

    fn targeting<R: Relation>(world: &World, host: Entity, target: Entity) -> bool {
        let host_is_targeting = world
            .get::<Edges>(host)
            .map(|edges| &edges.targets[R::CLEANUP_POLICY as usize])
            .and_then(|bucket| bucket.get(&RelationId::of::<R>()))
            .map_or(false, |set| set.contains(&target));

        let target_is_hosted = world
            .get::<Edges>(target)
            .map(|edges| &edges.hosts[R::CLEANUP_POLICY as usize])
            .and_then(|bucket| bucket.get(&RelationId::of::<R>()))
            .map_or(false, |set| set.contains(&host));

        if host_is_targeting != target_is_hosted {
            panic!("Asymmetric edge info");
        }

        host_is_targeting
    }

    #[test]
    fn set_unset() {
        #[derive(Relation)]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();
        let [host, target] = from_fn(|_| world.spawn_empty().id());

        world.entity_mut(host).set::<R>(target);
        assert!(targeting::<R>(&world, host, target));
        assert!(is_participant::<R>(&world, host));
        assert!(is_root::<R>(&world, target));

        world.entity_mut(host).unset::<R>(target);
        assert!(!has_edges(&world, target));
        assert!(!has_edges(&world, host));
        assert!(!is_participant::<R>(&world, host));
        assert!(!is_root::<R>(&world, target));
    }

    #[test]
    fn exclusive() {
        #[derive(Relation)]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();
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

        assert!(!has_edges(&world, t0));
        assert!(!is_root::<R>(&world, t0));
    }

    #[derive(Relation)]
    #[cleanup(policy = "Orphan")]
    struct Orphan;

    #[derive(Relation)]
    #[cleanup(policy = "Counted")]
    struct Counted;

    #[derive(Relation)]
    #[cleanup(policy = "Recursive")]
    struct Recursive;

    #[derive(Relation)]
    #[cleanup(policy = "Total")]
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

            assert!(!has_edges(world, self.hosts.orphan));
            assert!(!has_edges(world, self.targets.orphan));
            assert!(!is_participant::<Orphan>(world, self.hosts.orphan));
            assert!(!is_root::<Orphan>(world, self.targets.orphan));

            assert!(world.get_entity(self.targets.counted).is_none());
            assert!(!has_edges(world, self.hosts.counted));
            assert!(!is_participant::<Counted>(world, self.hosts.counted,));

            assert!(world.get_entity(self.hosts.recursive).is_none());
            assert!(!has_edges(world, self.targets.recursive));
            assert!(!is_root::<Recursive>(world, self.targets.recursive));

            assert!(world.get_entity(self.hosts.total).is_none());
            assert!(world.get_entity(self.targets.total).is_none());
        }
    }

    #[test]
    fn orphan_in_despawned() {
        #[derive(Relation)]
        #[cleanup(policy = "Orphan")]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        world
            .spawn_empty()
            .set::<R>(test.center)
            .unwrap()
            .checked_despawn();

        test.assert_unchanged(&world);
        assert!(!is_participant::<R>(&world, test.center));
    }

    #[test]
    fn orphan_out_despawned() {
        #[derive(Relation)]
        #[cleanup(policy = "Orphan")]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.entity_mut(test.center).set::<R>(e);

        world.entity_mut(e).checked_despawn();
        test.assert_unchanged(&world);
        assert!(!is_participant::<R>(&world, test.center));
    }

    #[test]
    fn counted_in_despawned() {
        #[derive(Relation)]
        #[cleanup(policy = "Counted")]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        world
            .spawn_empty()
            .set::<R>(test.center)
            .unwrap()
            .checked_despawn();

        test.assert_cleaned(&world);
    }

    #[test]
    fn counted_out_despawned() {
        #[derive(Relation)]
        #[cleanup(policy = "Counted")]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.entity_mut(test.center).set::<R>(e);

        world.entity_mut(e).checked_despawn();
        test.assert_unchanged(&world);
        assert!(!is_participant::<R>(&world, test.center));
    }

    #[test]
    fn recursive_in_despawned() {
        #[derive(Relation)]
        #[cleanup(policy = "Recursive")]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        world
            .spawn_empty()
            .set::<R>(test.center)
            .unwrap()
            .checked_despawn();

        test.assert_unchanged(&world);
        assert!(!is_participant::<R>(&world, test.center));
    }

    #[test]
    fn recursive_out_despawned() {
        #[derive(Relation)]
        #[cleanup(policy = "Recursive")]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.entity_mut(test.center).set::<R>(e);

        world.entity_mut(e).checked_despawn();
        test.assert_cleaned(&world);
    }

    #[test]
    fn total_in_despawned() {
        #[derive(Relation)]
        #[cleanup(policy = "Total")]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        world
            .spawn_empty()
            .set::<R>(test.center)
            .unwrap()
            .checked_despawn();

        test.assert_cleaned(&world);
    }

    #[test]
    fn total_out_despawned() {
        #[derive(Relation)]
        #[cleanup(policy = "Total")]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.entity_mut(test.center).set::<R>(e);

        world.entity_mut(e).checked_despawn();
        test.assert_cleaned(&world);
    }

    #[test]
    fn orphan_in_unset() {
        #[derive(Relation)]
        #[cleanup(policy = "Orphan")]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        world
            .spawn_empty()
            .set::<R>(test.center)
            .unwrap()
            .unset::<R>(test.center);

        test.assert_unchanged(&world);
        assert!(!is_participant::<R>(&world, test.center));
    }

    #[test]
    fn orphan_out_unset() {
        #[derive(Relation)]
        #[cleanup(policy = "Orphan")]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();

        world
            .entity_mut(test.center)
            .set::<R>(e)
            .unwrap()
            .unset::<R>(e);

        test.assert_unchanged(&world);
        assert!(!is_participant::<R>(&world, test.center));
    }

    #[test]
    fn counted_in_unset() {
        #[derive(Relation)]
        #[cleanup(policy = "Counted")]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        world
            .spawn_empty()
            .set::<R>(test.center)
            .unwrap()
            .unset::<R>(test.center);

        test.assert_cleaned(&world);
    }

    #[test]
    fn counted_out_unset() {
        #[derive(Relation)]
        #[cleanup(policy = "Counted")]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();

        world
            .entity_mut(test.center)
            .set::<R>(e)
            .unwrap()
            .unset::<R>(e);

        test.assert_unchanged(&world);
        assert!(!is_participant::<R>(&world, test.center));
    }

    #[test]
    fn recursive_in_unset() {
        #[derive(Relation)]
        #[cleanup(policy = "Recursive")]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        world
            .spawn_empty()
            .set::<R>(test.center)
            .unwrap()
            .unset::<R>(test.center);

        test.assert_unchanged(&world);
        assert!(!is_participant::<R>(&world, test.center));
    }

    #[test]
    #[should_panic]
    fn recursive_out_unset() {
        #[derive(Relation)]
        #[cleanup(policy = "Recursive")]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();

        world
            .entity_mut(test.center)
            .set::<R>(e)
            .unwrap()
            .unset::<R>(e);

        test.assert_cleaned(&world);
    }

    #[test]
    fn total_in_unset() {
        #[derive(Relation)]
        #[cleanup(policy = "Total")]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        world
            .spawn_empty()
            .set::<R>(test.center)
            .unwrap()
            .unset::<R>(test.center);

        test.assert_cleaned(&world);
    }

    #[test]
    #[should_panic]
    fn total_out_unset() {
        #[derive(Relation)]
        #[cleanup(policy = "Total")]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();

        world
            .entity_mut(test.center)
            .set::<R>(e)
            .unwrap()
            .unset::<R>(e);

        test.assert_cleaned(&world);
    }
}*/