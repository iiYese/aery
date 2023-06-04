use bevy::{
    ecs::{
        component::Component,
        entity::Entity,
        query::{Or, With, WorldQuery},
        system::{Command, Resource},
        world::World,
    },
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

/// Supported cleanup patterns. Cleanup is triggered when the entities participating in a
/// relationship change. Ie.
/// - When an entity is despawned
/// - When the relation is removed
#[derive(Clone, Copy)]
pub enum CleanupPolicy {
    /// Will do no further cleanup.
    Orphan,
    /// Counted relationships "count" the number of fosters they have. If it ever reaches zero they
    /// will delete themselves. This is effectively reference counting.
    Counted,
    /// When targets of recursively cleaning relations are deleted they also delete all their
    /// fosters. Unsetting a recursively cleaning relation is the same as despawning the foster.
    Recursive,
    /// Total performs both counted and recursive cleanup.
    Total,
}

/// The relation trait. This is what controls the cleanup policy and exclusivity of a relation.
pub trait Relation: 'static + Send + Sync {
    const CLEANUP_POLICY: CleanupPolicy = CleanupPolicy::Orphan;
    const EXCLUSIVE: bool = true;
}

#[derive(Component, Default)]
pub(crate) struct Edges {
    pub fosters: [HashMap<TypeId, IndexSet<Entity>>; 4],
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
    pub(crate) fn iter_fosters<R: Relation>(&self) -> EdgeIter<'_> {
        self.fosters[R::CLEANUP_POLICY as usize]
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

    let (has_fosters, has_targets) = (
        edges.fosters[R::CLEANUP_POLICY as usize]
            .get(&TypeId::of::<R>())
            .map(|fosters| !fosters.is_empty())
            .unwrap_or(false),
        edges.targets[R::CLEANUP_POLICY as usize]
            .get(&TypeId::of::<R>())
            .map(|targets| !targets.is_empty())
            .unwrap_or(false),
    );

    if !has_fosters {
        edges.fosters[R::CLEANUP_POLICY as usize].remove(&TypeId::of::<R>());
    }

    if !has_targets {
        edges.targets[R::CLEANUP_POLICY as usize].remove(&TypeId::of::<R>());
    }

    if edges
        .targets
        .iter()
        .chain(edges.fosters.iter())
        .all(HashMap::is_empty)
    {
        world.entity_mut(entity).remove::<Edges>();
    }

    match (has_fosters, has_targets) {
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

/// Command to set relationships between entities. Use convenience function [`set`].
pub struct Set<R>
where
    R: Relation,
{
    pub foster: Entity,
    pub target: Entity,
    pub _phantom: PhantomData<R>,
}

impl<R: Relation> Set<R> {
    pub fn new(foster: Entity, target: Entity) -> Self {
        Set {
            foster,
            target,
            _phantom: PhantomData,
        }
    }
}

impl<R> Command for Set<R>
where
    R: Relation,
{
    fn write(self, world: &mut World) {
        if self.foster == self.target {
            // TODO: Logging
            return;
        }

        if world.get_entity(self.target).is_none() {
            // TODO: Logging
            return;
        }

        if world.get_entity(self.foster).is_none() {
            // TODO: Logging
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

        target_edges.fosters[R::CLEANUP_POLICY as usize]
            .entry(TypeId::of::<R>())
            .or_default()
            .insert(self.foster);

        if !target_edges.targets[R::CLEANUP_POLICY as usize].contains_key(&TypeId::of::<R>()) {
            world.entity_mut(self.target).insert(RootMarker::<R> {
                _phantom: PhantomData,
            });
        }

        world.entity_mut(self.target).insert(target_edges);

        let mut foster_edges = world
            .entity_mut(self.foster)
            .remove::<RootMarker<R>>()
            .get_mut::<Edges>()
            .map(|mut edges| std::mem::take(&mut *edges))
            .unwrap_or_default();

        let old = foster_edges.targets[R::CLEANUP_POLICY as usize]
            .get(&TypeId::of::<R>())
            .and_then(|targets| targets.first())
            .copied();

        foster_edges.targets[R::CLEANUP_POLICY as usize]
            .entry(TypeId::of::<R>())
            .or_default()
            .insert(self.target);

        world.entity_mut(self.foster).insert((
            foster_edges,
            Participant::<R> {
                _phantom: PhantomData,
            },
        ));

        if let Some(old) = old.filter(|old| R::EXCLUSIVE && self.target != *old) {
            Command::write(
                Unset::<R> {
                    foster: self.foster,
                    target: old,
                    _phantom: PhantomData,
                },
                world,
            );
        }
    }
}

/// Command to remove relationships between entities.
pub struct Unset<R>
where
    R: Relation,
{
    pub foster: Entity,
    pub target: Entity,
    pub _phantom: PhantomData<R>,
}

impl<R: Relation> Command for Unset<R> {
    fn write(self, world: &mut World) {
        Command::write(
            UnsetErased {
                foster: self.foster,
                target: self.target,
                typeid: TypeId::of::<R>(),
                policy: R::CLEANUP_POLICY,
            },
            world,
        );
    }
}

struct UnsetErased {
    foster: Entity,
    target: Entity,
    typeid: TypeId,
    policy: CleanupPolicy,
}

impl Command for UnsetErased {
    fn write(self, world: &mut World) {
        let Some(refragment) = world
            .resource::<RefragmentHooks>()
            .hooks
            .get(&self.typeid)
            .copied()
        else {
            return
        };

        let Some(mut foster_edges) = world
            .get_mut::<Edges>(self.foster)
            .map(|mut edges| std::mem::take(&mut *edges))
        else {
            return
        };

        let Some(mut target_edges) = world
            .get_mut::<Edges>(self.foster)
            .map(|mut edges| std::mem::take(&mut *edges))
        else {
            world.entity_mut(self.foster).insert(foster_edges);
            return
        };

        foster_edges.targets[self.policy as usize]
            .entry(self.typeid)
            .and_modify(|fosters| {
                fosters.remove(&self.target);
            });

        target_edges.fosters[self.policy as usize]
            .entry(self.typeid)
            .and_modify(|fosters| {
                fosters.remove(&self.foster);
            });

        if foster_edges.targets[self.policy as usize]
            .get(&self.typeid)
            .map_or(false, IndexSet::is_empty)
        {
            foster_edges.targets[self.policy as usize].remove(&self.typeid);
        }

        let target_orphaned = target_edges.fosters[self.policy as usize]
            .get(&self.typeid)
            .map_or(false, IndexSet::is_empty);

        if target_orphaned {
            target_edges.fosters[self.policy as usize].remove(&self.typeid);
        }

        if foster_edges
            .targets
            .iter()
            .chain(foster_edges.fosters.iter())
            .all(HashMap::is_empty)
        {
            world.entity_mut(self.foster).remove::<Edges>();
        } else {
            world.entity_mut(self.foster).insert(foster_edges);
        }

        if target_edges
            .targets
            .iter()
            .chain(target_edges.fosters.iter())
            .any(|bucket| !bucket.is_empty())
        {
            world.entity_mut(self.target).insert(target_edges);
        } else {
            world.entity_mut(self.target).remove::<Edges>();
        }

        match self.policy {
            CleanupPolicy::Orphan => {
                refragment(world, self.foster);
                refragment(world, self.target);
            }
            CleanupPolicy::Recursive => {
                Command::write(
                    CheckedDespawn {
                        entity: self.foster,
                    },
                    world,
                );
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
                refragment(world, self.foster);
            }
            CleanupPolicy::Total => {
                Command::write(
                    CheckedDespawn {
                        entity: self.foster,
                    },
                    world,
                );
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

        while let Some(entity) = queue.pop_front() {
            let Ok(edges) = graph
                .get_mut(world, entity)
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

                let target_fosters = target_edges.fosters[CleanupPolicy::Total as usize]
                    .get_mut(typeid)
                    .unwrap();

                target_fosters.remove(target);

                if target_fosters.is_empty() {
                    to_despawn.insert(*target);
                }
            }

            for foster in edges.fosters[CleanupPolicy::Total as usize]
                .iter()
                .flat_map(|(_, fosters)| fosters.iter().copied())
            {
                queue.push_back(foster);
                to_despawn.insert(foster);
            }

            // Recursive relations
            for (typeid, target) in edges.targets[CleanupPolicy::Recursive as usize]
                .iter()
                .flat_map(|(typeid, targets)| targets.iter().map(move |target| (typeid, target)))
            {
                let Ok(mut target_edges) = graph.get_mut(world, *target) else {
                    continue
                };

                let target_fosters = target_edges.fosters[CleanupPolicy::Recursive as usize]
                    .get_mut(typeid)
                    .unwrap();

                target_fosters.remove(target);
                to_refrag.entry(*typeid).or_default().insert(*target);
            }

            for foster in edges.fosters[CleanupPolicy::Recursive as usize]
                .iter()
                .flat_map(|(_, fosters)| fosters.iter().copied())
            {
                queue.push_back(foster);
                to_despawn.insert(foster);
            }

            // Counted relations
            for (typeid, target) in edges.targets[CleanupPolicy::Counted as usize]
                .iter()
                .flat_map(|(typeid, targets)| targets.iter().map(move |target| (typeid, target)))
            {
                let Ok(mut target_edges) = graph.get_mut(world, *target) else {
                    continue
                };

                let target_fosters = target_edges.fosters[CleanupPolicy::Counted as usize]
                    .get_mut(typeid)
                    .unwrap();

                target_fosters.remove(target);

                if target_fosters.is_empty() {
                    to_despawn.insert(*target);
                }
            }

            for (typeid, foster) in edges.fosters[CleanupPolicy::Counted as usize]
                .iter()
                .flat_map(|(typeid, fosters)| fosters.iter().map(move |foster| (typeid, foster)))
            {
                let Ok(mut foster_edges) = graph.get_mut(world, *foster) else {
                    continue
                };

                foster_edges.targets[CleanupPolicy::Counted as usize]
                    .get_mut(typeid)
                    .unwrap()
                    .remove(&self.entity);

                to_refrag.entry(*typeid).or_default().insert(*foster);
            }

            // Orphaning relations
            for (typeid, target) in edges.targets[CleanupPolicy::Orphan as usize]
                .iter()
                .flat_map(|(typeid, targets)| targets.iter().map(move |target| (typeid, target)))
            {
                let Ok(mut target_edges) = graph.get_mut(world, *target) else {
                    continue
                };

                let target_fosters = target_edges.fosters[CleanupPolicy::Orphan as usize]
                    .get_mut(typeid)
                    .unwrap();

                target_fosters.remove(target);
                to_refrag.entry(*typeid).or_default().insert(*target);
            }

            for (typeid, foster) in edges.fosters[CleanupPolicy::Orphan as usize]
                .iter()
                .flat_map(|(typeid, fosters)| fosters.iter().map(move |foster| (typeid, foster)))
            {
                let Ok(mut foster_edges) = graph.get_mut(world, *foster) else {
                    continue
                };

                let foster_targets = foster_edges.targets[CleanupPolicy::Orphan as usize]
                    .get_mut(typeid)
                    .unwrap();

                foster_targets.remove(foster);
                to_refrag.entry(*typeid).or_default().insert(*foster);
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

#[cfg(test)]
mod tests {
    use crate::{prelude::*, relation::Edges};
    use bevy::{ecs::system::Command, prelude::*};
    use core::any::TypeId;
    use std::array::from_fn;

    struct R;

    impl Relation for R {}

    fn assert_targets<R: Relation>(world: &World, foster: Entity, target: Entity) {
        world
            .get::<Edges>(foster)
            .expect("Foster has no edges")
            .targets[R::CLEANUP_POLICY as usize]
            .get(&TypeId::of::<R>())
            .expect("Foster has no bucket entry for R")
            .get(&target)
            .expect("Foster does not target entity");

        world
            .get::<Edges>(target)
            .expect("Target has no edges")
            .fosters[R::CLEANUP_POLICY as usize]
            .get(&TypeId::of::<R>())
            .expect("Target has no bucket entry for R")
            .get(&foster)
            .expect("Target is not fostered");
    }

    #[test]
    fn set() {
        App::new()
            .add_plugin(Aery)
            .add_startup_system(|world: &mut World| {
                let [foster, target] = from_fn(|_| world.spawn_empty().id());
                Command::write(Set::<R>::new(foster, target), world);
                assert_targets::<R>(world, foster, target);
            });
    }
}
