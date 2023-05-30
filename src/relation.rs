use bevy::{
    ecs::{
        component::Component,
        entity::Entity,
        query::{Or, ReadOnlyWorldQuery, With, WorldQuery},
        world::World,
    },
    utils::{all_tuples, HashMap},
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

#[derive(Clone, Copy)]
pub enum CleanupPolicy {
    Orphan,
    Counted,
    Recursive,
    Total,
}

pub(crate) fn refragment<R: Relation>(world: &mut World, entity: Entity) {
    let Some(mut edges) = world
        .get_mut::<Edges>(entity)
    else {
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

pub trait Relation: 'static + Send + Sync {
    const CLEANUP_POLICY: CleanupPolicy = CleanupPolicy::Orphan;
    const EXCLUSIVE: bool = true;
}

#[derive(Component, Default)]
pub(crate) struct Edges {
    pub fosters: [HashMap<TypeId, IndexSet<Entity>>; 4],
    pub targets: [HashMap<TypeId, IndexSet<Entity>>; 4],
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
