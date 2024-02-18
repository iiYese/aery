use crate::{
    edges::EdgeIter,
    relation::{Hierarchy, Relation, RelationId},
    tuple_traits::*,
};

use bevy_ecs::{entity::Entity, query::QueryData};

use std::marker::PhantomData;

/// Struct to track metadat for a join operation.
pub struct JoinWith<Relations, JoinEdges, JoinItems> {
    pub(crate) relations: Relations,
    pub(crate) edges: PhantomData<JoinEdges>,
    pub(crate) items: JoinItems,
}

#[allow(missing_docs)]
pub struct SelfTracking;

/// Struct to track metadat for a traversal operation.
pub struct TraverseAnd<Control, Edge, Starts, Tracked = (), Init = (), Fold = ()> {
    pub(crate) control: Control,
    pub(crate) edge: PhantomData<Edge>,
    pub(crate) starts: Starts,
    pub(crate) track: Tracked,
    pub(crate) init: Init,
    pub(crate) fold: Fold,
}

/// [`WorldQuery`] type to query for Relation types. Takes a [`RelationSet`] which is a single
/// relation or tuple of relation types. *Must appear in the second position of the outer most tuple
/// to use relation operations and no type may appear more than once for operations to work.*
///
/// [`RelationSet`]: crate::tuple_traits::RelationSet
#[derive(QueryData)]
pub struct Relations<RS: RelationSet> {
    pub(crate) edges: RS::Edges,
    _phantom: PhantomData<RS>,
}

#[allow(missing_docs)]
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

/// Flip the direction of a [`Join`] or [`Traverse`] operation to use targets instead of hosts.
///
/// [`Join`]: crate::operations::Join
/// [`Traverse`]: crate::operations::Traverse
pub struct Up<R>(PhantomData<R>);

#[allow(missing_docs)]
pub trait EdgeSide {
    fn entities<'i, 'r, RS>(relations: &'r RelationsItem<'i, RS>) -> EdgeIter<'r>
    where
        'i: 'r,
        RS: RelationSet,
        RelationsItem<'i, RS>: RelationEntries;
}

impl EdgeSide for Hierarchy {
    fn entities<'i, 'r, RS>(relations: &'r RelationsItem<'i, RS>) -> EdgeIter<'r>
    where
        'i: 'r,
        RS: RelationSet,
        RelationsItem<'i, RS>: RelationEntries,
    {
        relations.hosts(Hierarchy).iter().copied()
    }
}

impl EdgeSide for Up<Hierarchy> {
    fn entities<'i, 'r, RS>(relations: &'r RelationsItem<'i, RS>) -> EdgeIter<'r>
    where
        'i: 'r,
        RS: RelationSet,
        RelationsItem<'i, RS>: RelationEntries,
    {
        relations.targets(Hierarchy).iter().copied()
    }
}

impl<R: Relation> EdgeSide for R {
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
    fn entities<'i, 'r, RS>(relations: &'r RelationsItem<'i, RS>) -> EdgeIter<'r>
    where
        'i: 'r,
        RS: RelationSet,
        RelationsItem<'i, RS>: RelationEntries,
    {
        relations.targets(RelationId::of::<R>()).iter().copied()
    }
}
