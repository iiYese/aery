#![allow(clippy::type_complexity)]
#![allow(clippy::too_many_arguments)]

use bevy::{
    ecs::{
        query::{ReadOnlyWorldQuery, WorldQuery},
        system::Command,
    },
    prelude::*,
    utils::HashMap,
};
use core::any::TypeId;

use indexmap::{IndexMap, IndexSet};
use std::marker::PhantomData;

pub mod ops;
pub mod relation;
mod tuple_traits;

pub use ops::*;
pub use relation::*;

pub struct Aery;

#[derive(Resource, Default)]
struct RefragmentHooks {
    hooks: HashMap<TypeId, fn(&mut World, Entity, Edges)>,
}

impl Plugin for Aery {
    fn build(&self, app: &mut App) {
        app.init_resource::<RefragmentHooks>();
    }
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
                UnSet::<R> {
                    foster: self.foster,
                    target: old,
                    _phantom: PhantomData,
                },
                world,
            );
        }
    }
}

pub struct UnSet<R>
where
    R: Relation,
{
    pub foster: Entity,
    pub target: Entity,
    pub _phantom: PhantomData<R>,
}

impl<R> Command for UnSet<R>
where
    R: Relation,
{
    fn write(self, world: &mut World) {
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

        foster_edges.targets[R::CLEANUP_POLICY as usize]
            .entry(TypeId::of::<R>())
            .and_modify(|fosters| {
                fosters.remove(&self.target);
            });

        target_edges.fosters[R::CLEANUP_POLICY as usize]
            .entry(TypeId::of::<R>())
            .and_modify(|fosters| {
                fosters.remove(&self.foster);
            });

        if foster_edges.targets[R::CLEANUP_POLICY as usize]
            .get(&TypeId::of::<R>())
            .map_or(false, IndexSet::is_empty)
        {
            foster_edges.targets[R::CLEANUP_POLICY as usize].remove(&TypeId::of::<R>());
        }

        let target_orphaned = target_edges.fosters[R::CLEANUP_POLICY as usize]
            .get(&TypeId::of::<R>())
            .map_or(false, IndexSet::is_empty);

        if target_orphaned {
            target_edges.fosters[R::CLEANUP_POLICY as usize].remove(&TypeId::of::<R>());
        }

        if foster_edges
            .targets
            .iter()
            .chain(foster_edges.fosters.iter())
            .any(|bucket| !bucket.is_empty())
        {
            world.entity_mut(self.foster).insert(foster_edges);
        }

        if target_edges
            .targets
            .iter()
            .chain(target_edges.fosters.iter())
            .any(|bucket| !bucket.is_empty())
        {
            world.entity_mut(self.target).insert(target_edges);
        }

        match R::CLEANUP_POLICY {
            CleanupPolicy::Orphan => {
                refragment::<R>(world, self.foster);
                refragment::<R>(world, self.target);
            }
            CleanupPolicy::Recursive => {
                Command::write(
                    CheckedDespawn {
                        entity: self.foster,
                    },
                    world,
                );
                refragment::<R>(world, self.target);
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
                refragment::<R>(world, self.foster);
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
    fn write(self, world: &mut World) {}
}
