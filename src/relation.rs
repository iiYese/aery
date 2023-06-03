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

/// Only tracked for relations with recursive cleanup.
#[derive(Component)]
pub(crate) struct Root<T: Relation> {
    pub _phantom: PhantomData<T>,
}

#[derive(Component)]
pub(crate) struct Participant<T: Relation> {
    pub _phantom: PhantomData<T>,
}

#[derive(WorldQuery)]
pub struct RootOf<R: Relation> {
    filter: With<Root<R>>,
}

#[derive(WorldQuery)]
pub struct Participates<R: Relation> {
    filter: Or<(With<Participant<R>>, With<Root<R>>)>,
}

#[derive(Clone, Copy)]
pub enum CleanupPolicy {
    Orphan,
    Counted,
    Recursive,
    Total,
}

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
                .remove::<Root<R>>()
                .insert(Participant::<R> {
                    _phantom: PhantomData,
                });
        }
        (true, false) => {
            world
                .entity_mut(entity)
                .remove::<Participant<R>>()
                .insert(Root::<R> {
                    _phantom: PhantomData,
                });
        }
        (false, false) => {
            world
                .entity_mut(entity)
                .remove::<(Participant<R>, Root<R>)>();
        }
    }
}

#[derive(Resource, Default)]
pub(crate) struct RefragmentHooks {
    hooks: HashMap<TypeId, fn(&mut World, Entity)>,
}

pub struct Set<R>
where
    R: Relation,
{
    pub foster: Entity,
    pub target: Entity,
    pub relation: R,
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
            world.entity_mut(self.target).insert(Root::<R> {
                _phantom: PhantomData,
            });
        }

        world.entity_mut(self.target).insert(target_edges);

        let mut foster_edges = world
            .entity_mut(self.foster)
            .remove::<Root<R>>()
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
