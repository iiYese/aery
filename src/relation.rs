use bevy::{
    ecs::{
        component::Component,
        entity::Entity,
        query::{Or, ReadOnlyWorldQuery, With, WorldQuery},
        world::World,
    },
    utils::all_tuples,
};
use core::any::TypeId;
use indexmap::{IndexMap, IndexSet};
use std::marker::PhantomData;

/// Only tracked for relations with recursive cleanup.
#[derive(Component)]
pub(crate) struct Root<T: Relation> {
    pub _phantom: PhantomData<T>,
}

#[derive(Component)]
pub(crate) struct Participant<T: Relation> {
    pub _phantom: PhantomData<T>,
}

pub enum CleanupPolicy {
    Orphan = 0,
    Recursive = 1,
    Counted = 2,
    Total = 3,
}

pub(crate) fn refragment<R: Relation>(world: &mut World, entity: Entity) {
    if world.get_entity(entity).is_none() {
        return;
    }

    let (has_fosters, has_targets) = world.get::<Edges>(entity).map_or((false, false), |edges| {
        (
            edges.fosters[R::CLEANUP_POLICY as usize]
                .get(&TypeId::of::<R>())
                .map(|fosters| !fosters.is_empty())
                .unwrap_or_default(),
            edges.targets[R::CLEANUP_POLICY as usize]
                .get(&TypeId::of::<R>())
                .map(|targets| !targets.is_empty())
                .unwrap_or_default(),
        )
    });

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

pub trait Relation: 'static + Send + Sync {
    const CLEANUP_POLICY: CleanupPolicy = CleanupPolicy::Orphan;
    const EXCLUSIVE: bool = true;
}

#[derive(Component, Default)]
pub(crate) struct Edges {
    pub fosters: [IndexMap<TypeId, IndexSet<Entity>>; 4],
    pub targets: [IndexMap<TypeId, IndexSet<Entity>>; 4],
}

impl Edges {
    pub(crate) fn iter_targets<R: Relation>(&self) -> impl '_ + Iterator<Item = Entity> {
        self.targets[R::CLEANUP_POLICY as usize]
            .get(&TypeId::of::<R>())
            .map(|targets| targets.iter().copied())
            .into_iter()
            .flatten()
    }
}

pub trait RelationSet {
    type Filters: ReadOnlyWorldQuery;
}

#[derive(WorldQuery)]
pub struct Participates<R: Relation> {
    filter: Or<(With<Participant<R>>, With<Root<R>>)>,
}

impl<R: Relation> RelationSet for R {
    type Filters = Participates<R>;
}

// TODO: All tuple
impl<P0: RelationSet> RelationSet for (P0,) {
    type Filters = (P0::Filters,);
}

impl<P0: RelationSet, P1: RelationSet> RelationSet for (P0, P1) {
    type Filters = (P0::Filters, P1::Filters);
}
