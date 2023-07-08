use bevy::{
    ecs::{
        component::Component,
        entity::Entity,
        query::{Or, With, WorldQuery},
        system::{Command, Commands, Resource},
        world::World,
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
    /// hosts. Untargetting a recursively cleaning relation is the same as despawning the host.
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
pub struct Target<R>
where
    R: Relation,
{
    pub host: Entity,
    pub target: Entity,
    pub _phantom: PhantomData<R>,
}

impl<R> Command for Target<R>
where
    R: Relation,
{
    #[allow(clippy::let_unit_value)]
    fn write(self, world: &mut World) {
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
            Command::write(
                Untarget::<R> {
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
pub struct Untarget<R>
where
    R: Relation,
{
    pub host: Entity,
    pub target: Entity,
    pub _phantom: PhantomData<R>,
}

impl<R: Relation> Command for Untarget<R> {
    #[allow(clippy::let_unit_value)]
    fn write(self, world: &mut World) {
        let _ = R::ZST_OR_PANIC;

        Command::write(
            UntargetErased {
                host: self.host,
                target: self.target,
                typeid: TypeId::of::<R>(),
                policy: R::CLEANUP_POLICY,
            },
            world,
        );
    }
}

struct UntargetErased {
    host: Entity,
    target: Entity,
    typeid: TypeId,
    policy: CleanupPolicy,
}

impl Command for UntargetErased {
    fn write(self, world: &mut World) {
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

        match self.policy {
            CleanupPolicy::Orphan => {
                refragment(world, self.host);
                refragment(world, self.target);
            }
            CleanupPolicy::Recursive => {
                Command::write(CheckedDespawn { entity: self.host }, world);
                refragment(world, self.target);
            }
            CleanupPolicy::Counted => {
                if target_orphaned {
                    Command::write(
                        CheckedDespawn {
                            entity: self.target,
                        },
                        world,
                    );
                }
                refragment(world, self.host);
            }
            CleanupPolicy::Total => {
                Command::write(CheckedDespawn { entity: self.host }, world);
                if target_orphaned {
                    Command::write(
                        CheckedDespawn {
                            entity: self.target,
                        },
                        world,
                    );
                }
            }
        }
    }
}

/// Command for entities to untarget all of their relations of a given type.
pub struct UntargetAll<R>
where
    R: Relation,
{
    pub entity: Entity,
    pub _phantom: PhantomData<R>,
}

impl<R: Relation> Command for UntargetAll<R> {
    #[allow(clippy::let_unit_value)]
    fn write(self, world: &mut World) {
        while let Some(target) = world
            .get::<Edges>(self.entity)
            .and_then(|edges| edges.targets[R::CLEANUP_POLICY as usize].get(&TypeId::of::<R>()))
            .and_then(|targets| targets.first())
            .copied()
        {
            let _ = R::ZST_OR_PANIC;

            Command::write(
                Untarget::<R> {
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
    fn write(self, world: &mut World) {
        while let Some(host) = world
            .get::<Edges>(self.entity)
            .and_then(|edges| edges.hosts[R::CLEANUP_POLICY as usize].get(&TypeId::of::<R>()))
            .and_then(|targets| targets.first())
            .copied()
        {
            let _ = R::ZST_OR_PANIC;

            Command::write(
                Untarget::<R> {
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
    fn write(self, world: &mut World) {
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
