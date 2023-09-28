use crate::{
    edges::EdgeIter,
    relation::{Relation, RelationId},
    tuple_traits::*,
};

use bevy::ecs::{entity::Entity, query::WorldQuery};

use std::marker::PhantomData;

pub struct SelfTracking;

/// Struct that is used to track metadata for relation operations.
pub struct Operations<
    Control,
    JoinedTypes = (),
    JoinedQueries = (),
    Traversal = (),
    Starts = (),
    TrackedQueries = (),
    TrackSelf = (),
    Init = (),
    Fold = (),
> {
    pub(crate) control: Control,
    pub(crate) joined_types: PhantomData<JoinedTypes>,
    pub(crate) joined_queries: JoinedQueries,
    pub(crate) traversal: PhantomData<Traversal>,
    pub(crate) start: Starts,
    pub(crate) tracked_queries: TrackedQueries,
    pub(crate) track_self: PhantomData<TrackSelf>,
    pub(crate) init: Init,
    pub(crate) fold: Fold,
}

/// `WorldQuery` type to query for Relation types. Takes a [`RelationSet`] which is a single
/// relation or tuple of relation types. *Must appear in the second position of the outer most tuple
/// to use relation operations and no type may appear more than once for operations to work.*
/// See [`AeryQueryExt`] for operations.
#[derive(WorldQuery)]
pub struct Relations<RS: RelationSet> {
    pub(crate) edges: RS::Edges,
    pub(crate) entity: Entity,
    _phantom: PhantomData<RS>,
}

/// Struct to track inner product iteration.
pub struct EdgeProduct<'a, const N: usize> {
    pub(crate) base_iterators: [EdgeIter<'a>; N],
    pub(crate) live_iterators: [EdgeIter<'a>; N],
    pub(crate) entities: [Option<Entity>; N],
}

impl<'a, const N: usize> EdgeProduct<'a, N> {
    pub(crate) fn advance(&mut self, prev_matches: [bool; N]) -> Option<[Entity; N]> {
        let n = prev_matches
            .iter()
            .enumerate()
            .find_map(|(n, matches)| (!matches).then_some(n))
            .unwrap_or(N);

        for i in (1..N).skip(n) {
            self.live_iterators[i] = self.base_iterators[i].clone();
            self.entities[i] = self.live_iterators[i].next();
        }

        'next_permutation: {
            for i in (1..N).take(n).rev() {
                if let Some(entity) = self.live_iterators[i].next() {
                    self.entities[i] = Some(entity);
                    break 'next_permutation;
                } else {
                    self.live_iterators[i] = self.base_iterators[i].clone();
                    self.entities[i] = self.live_iterators[i].next();
                }
            }

            self.entities[0] = self.live_iterators[0].next();
        }

        self.entities
            .iter()
            .all(Option::is_some)
            .then(|| self.entities.map(Option::unwrap))
    }
}

pub struct Up<R>(PhantomData<R>);

pub trait EdgeSide {
    const DESCENT_OR_PANIC: ();

    fn entities<'i, 'r, RS>(relations: &'r RelationsItem<'i, RS>) -> EdgeIter<'r>
    where
        'i: 'r,
        RS: RelationSet,
        RelationsItem<'i, RS>: RelationEntries;
}

impl<R: Relation> EdgeSide for R {
    const DESCENT_OR_PANIC: () = ();

    fn entities<'i, 'r, RS>(relations: &'r RelationsItem<'i, RS>) -> EdgeIter<'r>
    where
        'i: 'r,
        RS: RelationSet,
        RelationsItem<'i, RS>: RelationEntries,
    {
        relations.hosts(RelationId::of::<R>()).iter().copied()
    }
}

impl<R: Relation> EdgeSide for Up<R> {
    const DESCENT_OR_PANIC: () = {
        panic!("Operation not supported for ascent");
    };

    fn entities<'i, 'r, RS>(relations: &'r RelationsItem<'i, RS>) -> EdgeIter<'r>
    where
        'i: 'r,
        RS: RelationSet,
        RelationsItem<'i, RS>: RelationEntries,
    {
        relations.targets(RelationId::of::<R>()).iter().copied()
    }
}
