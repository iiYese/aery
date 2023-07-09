use bevy::{
    ecs::{
        component::Component,
        entity::Entity,
        query::{Or, With, WorldQuery},
        system::{Command, Resource},
        world::{EntityMut, World},
    },
    log::warn,
    utils::{HashMap, HashSet},
};

use core::any::TypeId;
use indexmap::IndexSet;
use std::{collections::VecDeque, marker::PhantomData};

#[derive(Component)]
pub(crate) struct RootMarker<R: Relation> {
    pub _phantom: PhantomData<R>,
}

#[derive(Component)]
pub(crate) struct Participant<R: Relation> {
    pub _phantom: PhantomData<R>,
}

/// Filter to find roots of a relationship graph.
#[derive(WorldQuery)]
pub struct Root<R: Relation> {
    filter: With<RootMarker<R>>,
}

/// Filter to find any participants of a relationship.
#[derive(WorldQuery)]
pub struct Participates<R: Relation> {
    filter: Or<(With<Participant<R>>, With<RootMarker<R>>)>,
}

/// Supported cleanup patterns. When entities have multiple relations with different cleanup
/// policies each relation looks at the graph as if it were the only relation that existed.
/// In effect the summation of their cleanup is applied.
/// Cleanup is triggered when the entities participating in a relationship change. Ie.
/// - When an entity is despawned
/// - When the relation is removed
/// # Example
/// ```
/// use bevy::prelude::*;
/// use aery::prelude::*;
///
/// #[derive(Relation)]
/// #[cleanup(policy = "Orphan")]
/// struct O;
///
/// #[derive(Relation)]
/// #[cleanup(policy = "Recursive")]
/// struct R;
///
/// fn sys(world: &mut World) {
///     let [e0, e1, e2, e3, e4, e5, e6] = std::array::from_fn(|_| world.spawn_empty().id());
///
///     world.set::<O>(e1, e0);
///     world.set::<O>(e2, e0);
///     world.set::<O>(e3, e1);
///     world.set::<O>(e4, e1);
///
///     world.set::<R>(e1, e0);
///     world.set::<R>(e4, e1);
///     world.set::<R>(e5, e2);
///     world.set::<R>(e6, e2);
///
///     // Results in:
///     //             0
///     //           // \
///     //          //   \
///     //         RO     O
///     //        //       \
///     //       //         \
///     //      1            2
///     //     / \\         / \
///     //    O   RO       R   R
///     //   /     \\     /     \
///     //  3       4    5       6
///
///     world.checked_despawn(e0);
///
///     // After cleanup:
///     //                   2
///     //                  / \
///     //      3          R   R
///     //                /     \
///     //               5       6
/// }
/// ```
#[derive(Clone, Copy)]
pub enum CleanupPolicy {
    /// Will do no further cleanup.
    Orphan,
    /// Counted relationships "count" the number of hosts they have. If it ever reaches zero they
    /// will delete themselves. This is effectively reference counting.
    Counted,
    /// When targets of recursively cleaning relations are deleted they also delete all their
    /// hosts. Unsetting a recursively cleaning relation is the same as despawning the host.
    Recursive,
    /// Total performs both counted and recursive cleanup.
    Total,
}

pub trait ZstOrPanic: Sized {
    const ZST_OR_PANIC: () = {
        // TODO: Make diagnostic friendlier when `std::any::type_name` becomes const
        // TODO: Use actual type level mechanism and remove hack when possible in stable
        if std::mem::size_of::<Self>() != 0 {
            panic!("Not a ZST")
        }
    };
}

impl<T> ZstOrPanic for T {}

/// The relation trait. This is what controls the cleanup policy and exclusivity of a relation.
pub trait Relation: 'static + Sized + Send + Sync {
    const CLEANUP_POLICY: CleanupPolicy = CleanupPolicy::Orphan;
    const EXCLUSIVE: bool = true;
}

#[derive(Component, Default, Debug)]
pub(crate) struct Edges {
    pub hosts: [HashMap<TypeId, IndexSet<Entity>>; 4],
    pub targets: [HashMap<TypeId, IndexSet<Entity>>; 4],
}

#[derive(WorldQuery)]
pub struct EdgeWQ {
    pub(crate) edges: &'static Edges,
}

type EdgeIter<'a> = std::iter::Flatten<
    std::option::IntoIter<std::iter::Copied<indexmap::set::Iter<'a, bevy::prelude::Entity>>>,
>;

impl Edges {
    pub(crate) fn iter_hosts<R: Relation>(&self) -> EdgeIter<'_> {
        self.hosts[R::CLEANUP_POLICY as usize]
            .get(&TypeId::of::<R>())
            .map(|targets| targets.iter().copied())
            .into_iter()
            .flatten()
    }

    pub(crate) fn iter_targets<R: Relation>(&self) -> EdgeIter<'_> {
        self.targets[R::CLEANUP_POLICY as usize]
            .get(&TypeId::of::<R>())
            .map(|targets| targets.iter().copied())
            .into_iter()
            .flatten()
    }
}

pub(crate) fn refragment<R: Relation>(world: &mut World, entity: Entity) {
    let Some(mut edges) = world.get_mut::<Edges>(entity) else {
        return
    };

    let (has_hosts, has_targets) = (
        edges.hosts[R::CLEANUP_POLICY as usize]
            .get(&TypeId::of::<R>())
            .map(|hosts| !hosts.is_empty())
            .unwrap_or(false),
        edges.targets[R::CLEANUP_POLICY as usize]
            .get(&TypeId::of::<R>())
            .map(|targets| !targets.is_empty())
            .unwrap_or(false),
    );

    if !has_hosts {
        edges.hosts[R::CLEANUP_POLICY as usize].remove(&TypeId::of::<R>());
    }

    if !has_targets {
        edges.targets[R::CLEANUP_POLICY as usize].remove(&TypeId::of::<R>());
    }

    if edges
        .targets
        .iter()
        .chain(edges.hosts.iter())
        .all(HashMap::is_empty)
    {
        world.entity_mut(entity).remove::<Edges>();
    }

    match (has_hosts, has_targets) {
        (_, true) => {
            world
                .entity_mut(entity)
                .remove::<RootMarker<R>>()
                .insert(Participant::<R> {
                    _phantom: PhantomData,
                });
        }
        (true, false) => {
            world
                .entity_mut(entity)
                .remove::<Participant<R>>()
                .insert(RootMarker::<R> {
                    _phantom: PhantomData,
                });
        }
        (false, false) => {
            world
                .entity_mut(entity)
                .remove::<(Participant<R>, RootMarker<R>)>();
        }
    }
}

#[derive(Resource, Default)]
pub(crate) struct RefragmentHooks {
    hooks: HashMap<TypeId, fn(&mut World, Entity)>,
}

/// Command to set relationship target for entities. If either of the participants do not exist or
/// the host tries to target itself the operation will be ignored and logged.
pub struct Set<R>
where
    R: Relation,
{
    pub host: Entity,
    pub target: Entity,
    pub _phantom: PhantomData<R>,
}

impl<R> Command for Set<R>
where
    R: Relation,
{
    #[allow(clippy::let_unit_value)]
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

        world
            .resource_mut::<RefragmentHooks>()
            .hooks
            .insert(TypeId::of::<R>(), refragment::<R>);

        let mut target_edges = world
            .get_mut::<Edges>(self.target)
            .map(|mut edges| std::mem::take(&mut *edges))
            .unwrap_or_default();

        target_edges.hosts[R::CLEANUP_POLICY as usize]
            .entry(TypeId::of::<R>())
            .or_default()
            .insert(self.host);

        if !target_edges.targets[R::CLEANUP_POLICY as usize].contains_key(&TypeId::of::<R>()) {
            world.entity_mut(self.target).insert(RootMarker::<R> {
                _phantom: PhantomData,
            });
        }

        world.entity_mut(self.target).insert(target_edges);

        let mut host_edges = world
            .entity_mut(self.host)
            .remove::<RootMarker<R>>()
            .get_mut::<Edges>()
            .map(|mut edges| std::mem::take(&mut *edges))
            .unwrap_or_default();

        let old = host_edges.targets[R::CLEANUP_POLICY as usize]
            .get(&TypeId::of::<R>())
            .and_then(|targets| targets.first())
            .copied();

        host_edges.targets[R::CLEANUP_POLICY as usize]
            .entry(TypeId::of::<R>())
            .or_default()
            .insert(self.target);

        world.entity_mut(self.host).insert((
            host_edges,
            Participant::<R> {
                _phantom: PhantomData,
            },
        ));

        if let Some(old) = old.filter(|old| R::EXCLUSIVE && self.target != *old) {
            Command::apply(
                Unset::<R> {
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
    pub host: Entity,
    pub target: Entity,
    pub _phantom: PhantomData<R>,
}

impl<R: Relation> Command for Unset<R> {
    #[allow(clippy::let_unit_value)]
    fn apply(self, world: &mut World) {
        let _ = R::ZST_OR_PANIC;

        Command::apply(
            UnsetErased {
                host: self.host,
                target: self.target,
                typeid: TypeId::of::<R>(),
                policy: R::CLEANUP_POLICY,
            },
            world,
        );
    }
}

struct UnsetErased {
    host: Entity,
    target: Entity,
    typeid: TypeId,
    policy: CleanupPolicy,
}

impl Command for UnsetErased {
    fn apply(self, world: &mut World) {
        let Some(refragment) = world
            .resource::<RefragmentHooks>()
            .hooks
            .get(&self.typeid)
            .copied()
        else {
            return
        };

        let Some(mut host_edges) = world
            .get_mut::<Edges>(self.host)
            .map(|mut edges| std::mem::take(&mut *edges))
        else {
            return
        };

        let Some(mut target_edges) = world
            .get_mut::<Edges>(self.target)
            .map(|mut edges| std::mem::take(&mut *edges))
        else {
            world.entity_mut(self.host).insert(host_edges);
            return
        };

        host_edges.targets[self.policy as usize]
            .entry(self.typeid)
            .and_modify(|hosts| {
                hosts.remove(&self.target);
            });

        target_edges.hosts[self.policy as usize]
            .entry(self.typeid)
            .and_modify(|hosts| {
                hosts.remove(&self.host);
            });

        let target_orphaned = target_edges.hosts[self.policy as usize]
            .get(&self.typeid)
            .map_or(false, IndexSet::is_empty);

        world.entity_mut(self.host).insert(host_edges);
        world.entity_mut(self.target).insert(target_edges);

        if target_orphaned && matches!(self.policy, CleanupPolicy::Counted | CleanupPolicy::Total) {
            Command::apply(
                CheckedDespawn {
                    entity: self.target,
                },
                world,
            );
        }

        refragment(world, self.host);
        refragment(world, self.target);
    }
}

/// Command for entities to untarget all of their relations of a given type.
pub struct UnsetAll<R>
where
    R: Relation,
{
    pub entity: Entity,
    pub _phantom: PhantomData<R>,
}

impl<R: Relation> Command for UnsetAll<R> {
    #[allow(clippy::let_unit_value)]
    fn apply(self, world: &mut World) {
        while let Some(target) = world
            .get::<Edges>(self.entity)
            .and_then(|edges| edges.targets[R::CLEANUP_POLICY as usize].get(&TypeId::of::<R>()))
            .and_then(|targets| targets.first())
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
    pub entity: Entity,
    pub _phantom: PhantomData<R>,
}

impl<R: Relation> Command for Withdraw<R> {
    #[allow(clippy::let_unit_value)]
    fn apply(self, world: &mut World) {
        while let Some(host) = world
            .get::<Edges>(self.entity)
            .and_then(|edges| edges.hosts[R::CLEANUP_POLICY as usize].get(&TypeId::of::<R>()))
            .and_then(|targets| targets.first())
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

/// Command to despawn entities with rleations. Despawning via any other method can lead to
/// dangling which will not produce correct behavior!
pub struct CheckedDespawn {
    pub entity: Entity,
}

impl Command for CheckedDespawn {
    fn apply(self, world: &mut World) {
        let mut to_refrag = HashMap::<TypeId, HashSet<Entity>>::new();
        let mut to_despawn = HashSet::<Entity>::from([self.entity]);
        let mut queue = VecDeque::from([self.entity]);

        let mut graph = world.query::<&mut Edges>();

        while let Some(curr) = queue.pop_front() {
            let Ok(edges) = graph
                .get_mut(world, curr)
                .map(|mut edges| std::mem::take(&mut *edges))
            else {
                continue
            };

            // Total relations
            for (typeid, target) in edges.targets[CleanupPolicy::Total as usize]
                .iter()
                .flat_map(|(typeid, targets)| targets.iter().map(move |target| (typeid, target)))
            {
                let Ok(mut target_edges) = graph.get_mut(world, *target) else {
                    continue
                };

                let Some(target_hosts) = target_edges
                    .hosts[CleanupPolicy::Total as usize]
                    .get_mut(typeid)
                else {
                    continue
                };

                target_hosts.remove(&curr);

                if target_hosts.is_empty() {
                    queue.push_back(*target);
                    to_despawn.insert(*target);
                }
            }

            for (typeid, host) in edges.hosts[CleanupPolicy::Total as usize]
                .iter()
                .flat_map(|(typeid, hosts)| hosts.iter().map(move |host| (typeid, host)))
            {
                let Ok(mut host_edges) = graph.get_mut(world, *host) else {
                    continue
                };

                host_edges.targets[CleanupPolicy::Total as usize]
                    .entry(*typeid)
                    .and_modify(|bucket| {
                        bucket.remove(&curr);
                    });

                queue.push_back(*host);
                to_despawn.insert(*host);
            }

            // Recursive relations
            for (typeid, target) in edges.targets[CleanupPolicy::Recursive as usize]
                .iter()
                .flat_map(|(typeid, targets)| targets.iter().map(move |target| (typeid, target)))
            {
                let Ok(mut target_edges) = graph.get_mut(world, *target) else {
                    continue
                };

                let Some(target_hosts) = target_edges
                    .hosts[CleanupPolicy::Recursive as usize]
                    .get_mut(typeid)
                else {
                    continue
                };

                target_hosts.remove(&curr);
                to_refrag.entry(*typeid).or_default().insert(*target);
            }

            for (typeid, host) in edges.hosts[CleanupPolicy::Recursive as usize]
                .iter()
                .flat_map(|(typeid, hosts)| hosts.iter().map(move |host| (typeid, host)))
            {
                let Ok(mut host_edges) = graph.get_mut(world, *host) else {
                    continue
                };

                host_edges.targets[CleanupPolicy::Recursive as usize]
                    .entry(*typeid)
                    .and_modify(|bucket| {
                        bucket.remove(&curr);
                    });

                queue.push_back(*host);
                to_despawn.insert(*host);
            }

            // Counted relations
            for (typeid, target) in edges.targets[CleanupPolicy::Counted as usize]
                .iter()
                .flat_map(|(typeid, targets)| targets.iter().map(move |target| (typeid, target)))
            {
                let Ok(mut target_edges) = graph.get_mut(world, *target) else {
                    continue
                };

                let Some(target_hosts) = target_edges
                    .hosts[CleanupPolicy::Counted as usize]
                    .get_mut(typeid)
                else {
                    continue
                };

                target_hosts.remove(&curr);

                if target_hosts.is_empty() {
                    queue.push_back(*target);
                    to_despawn.insert(*target);
                }
            }

            for (typeid, host) in edges.hosts[CleanupPolicy::Counted as usize]
                .iter()
                .flat_map(|(typeid, hosts)| hosts.iter().map(move |host| (typeid, host)))
            {
                let Ok(mut host_edges) = graph.get_mut(world, *host) else {
                    continue
                };

                host_edges.targets[CleanupPolicy::Counted as usize]
                    .entry(*typeid)
                    .and_modify(|bucket| {
                        bucket.remove(&curr);
                    });

                to_refrag.entry(*typeid).or_default().insert(*host);
            }

            // Orphaning relations
            for (typeid, target) in edges.targets[CleanupPolicy::Orphan as usize]
                .iter()
                .flat_map(|(typeid, targets)| targets.iter().map(move |target| (typeid, target)))
            {
                let Ok(mut target_edges) = graph.get_mut(world, *target) else {
                    continue
                };

                target_edges.hosts[CleanupPolicy::Orphan as usize]
                    .entry(*typeid)
                    .and_modify(|bucket| {
                        bucket.remove(&curr);
                    });

                to_refrag.entry(*typeid).or_default().insert(*target);
            }

            for (typeid, host) in edges.hosts[CleanupPolicy::Orphan as usize]
                .iter()
                .flat_map(|(typeid, hosts)| hosts.iter().map(move |host| (typeid, host)))
            {
                let Ok(mut host_edges) = graph.get_mut(world, *host) else {
                    continue
                };

                host_edges.targets[CleanupPolicy::Orphan as usize]
                    .entry(*typeid)
                    .and_modify(|bucket| {
                        bucket.remove(&curr);
                    });

                to_refrag.entry(*typeid).or_default().insert(*host);
            }
        }

        for entity in to_despawn {
            world.despawn(entity);
        }

        for (typeid, entities) in to_refrag {
            let refrag = world
                .resource::<RefragmentHooks>()
                .hooks
                .get(&typeid)
                .copied()
                .unwrap();

            for entity in entities {
                refrag(world, entity);
            }
        }
    }
}

pub trait RelationCommands: Sized {
    fn set<R: Relation>(&mut self, target: Entity) -> &'_ mut Self;
    fn unset<R: Relation>(self, target: Entity) -> Option<Self>;
    fn unset_all<R: Relation>(self) -> Option<Self>;
    fn withdraw<R: Relation>(self) -> Option<Self>;
    fn checked_despawn(self);
}

#[rustfmt::skip]
#[allow(clippy::let_unit_value)]
impl RelationCommands for EntityMut<'_> {
    fn set<R: Relation>(&mut self, target: Entity) -> &'_ mut Self {
        let _ = R::ZST_OR_PANIC;

        let id = self.id();

        self.world_scope(|world| Command::apply(
            Set::<R> { host: id, target, _phantom: PhantomData },
            world,
        ));

        self
    }

    fn unset<R: Relation>(self, target: Entity) -> Option<Self> {
        let _ = R::ZST_OR_PANIC;

        let id = self.id();
        let world = self.into_world_mut();

        Command::apply(
            Unset::<R> { host: id, target, _phantom: PhantomData },
            world,
        );

        world.get_entity_mut(id)
    }

    fn unset_all<R: Relation>(self) -> Option<Self> {
        let _ = R::ZST_OR_PANIC;

        let id = self.id();
        let world = self.into_world_mut();

        Command::apply(
            UnsetAll::<R> { entity: id, _phantom: PhantomData },
            world,
        );

        world.get_entity_mut(id)
    }

    fn withdraw<R: Relation>(self) -> Option<Self> {
        let _ = R::ZST_OR_PANIC;

        let id = self.id();
        let world = self.into_world_mut();

        Command::apply(
            Withdraw::<R> { entity: id, _phantom: PhantomData },
            world,
        );

        world.get_entity_mut(id)
    }

    fn checked_despawn(self) {
        let id = self.id();
        let world = self.into_world_mut();
        Command::apply(CheckedDespawn { entity: id }, world);
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        prelude::*,
        relation::{Edges, Participant, RefragmentHooks, RootMarker},
    };
    use bevy::prelude::*;
    use core::any::TypeId;
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
            .and_then(|bucket| bucket.get(&TypeId::of::<R>()))
            .map_or(false, |set| set.contains(&target));

        let target_is_hosted = world
            .get::<Edges>(target)
            .map(|edges| &edges.hosts[R::CLEANUP_POLICY as usize])
            .and_then(|bucket| bucket.get(&TypeId::of::<R>()))
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

        let mut e = world.spawn_empty();
        e.set::<R>(test.center);

        e.checked_despawn();
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

        let mut e = world.spawn_empty();
        e.set::<R>(test.center);

        e.checked_despawn();
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

        let mut e = world.spawn_empty();
        e.set::<R>(test.center);

        e.checked_despawn();
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

        let mut e = world.spawn_empty();
        e.set::<R>(test.center);

        e.checked_despawn();
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

        let mut e = world.spawn_empty();
        e.set::<R>(test.center);
        e.unset::<R>(test.center);

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
        let mut c = world.entity_mut(test.center);
        c.set::<R>(e);
        c.unset::<R>(e);

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

        let mut e = world.spawn_empty();
        e.set::<R>(test.center);
        e.unset::<R>(test.center);

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
        let mut c = world.entity_mut(test.center);
        c.set::<R>(e);
        c.unset::<R>(e);

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

        let mut e = world.spawn_empty();
        e.set::<R>(test.center);
        e.unset::<R>(test.center);

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
        let mut c = world.entity_mut(test.center);
        c.set::<R>(e);
        c.unset::<R>(e);

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

        let mut e = world.spawn_empty();
        e.set::<R>(test.center);
        e.unset::<R>(test.center);

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
        let mut c = world.entity_mut(test.center);
        c.set::<R>(e);
        c.unset::<R>(e);

        test.assert_cleaned(&world);
    }
}
