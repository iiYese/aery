use crate::operations::utils::{EdgeSide, JoinWith, Relations, RelationsItem, TraverseAnd};
use crate::tuple_traits::*;

use bevy::ecs::{
    entity::Entity,
    query::{ReadOnlyWorldQuery, WorldQuery},
    system::Query,
};

use std::{borrow::Borrow, collections::VecDeque};

//mod for_each_2arity;
//mod for_each_3arity;
//mod for_each_4arity;

//pub use for_each_2arity::*;
//pub use for_each_3arity::ForEachPermutations3Arity;
//pub use for_each_4arity::*;

/// Traversal Control FLow
pub enum TCF {
    Continue,
    Probe,
    Conclude,
    Exit,
}

impl From<()> for TCF {
    fn from(_: ()) -> Self {
        Self::Continue
    }
}

/// Join Control FLow
pub enum JCF {
    Continue,
    FastForward(usize),
    Exit,
}

impl From<()> for JCF {
    fn from(_: ()) -> Self {
        Self::Continue
    }
}

// ----------
// Traversals
// ----------

pub trait BasicTraversalForEach {
    type P0<'p0>;

    fn for_each<Func, Ret>(self, func: Func)
    where
        Ret: Into<TCF>,
        Func: for<'f, 'p0> FnMut(&'f mut Self::P0<'p0>) -> Ret;
}

impl<Q, RS, F, Edge: EdgeSide> BasicTraversalForEach
    for TraverseAnd<&'_ Query<'_, '_, (Q, Relations<RS>), F>, Edge>
where
    Q: WorldQuery,
    RS: RelationSet,
    F: ReadOnlyWorldQuery,
    Edge: EdgeSide,
    for<'i> RelationsItem<'i, RS>: RelationEntries,
{
    type P0<'p0> = (
        <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'p0>,
        RelationsItem<'p0, RS>,
    );

    fn for_each<Func, Ret>(self, mut func: Func)
    where
        Ret: Into<TCF>,
        Func: for<'f, 'p0> FnMut(&'f mut Self::P0<'p0>) -> Ret,
    {
        let mut queue = VecDeque::from([self.start]);

        'queue: while let Some(entity) = queue.pop_front() {
            let Ok(mut curr) = self.control.get(entity) else {
                continue;
            };

            match func(&mut curr).into() {
                TCF::Continue => {}
                TCF::Exit => return,
                TCF::Probe => {
                    queue.clear();
                }
                TCF::Conclude => {
                    continue 'queue;
                }
            }

            queue.extend(Edge::entities(&curr.1));
        }
    }
}

impl<Q, RS, F, Edge: EdgeSide> BasicTraversalForEach
    for TraverseAnd<&'_ mut Query<'_, '_, (Q, Relations<RS>), F>, Edge>
where
    Q: WorldQuery,
    RS: RelationSet,
    F: ReadOnlyWorldQuery,
    Edge: EdgeSide,
    for<'i> RelationsItem<'i, RS>: RelationEntries,
{
    type P0<'p0> = (<Q as WorldQuery>::Item<'p0>, RelationsItem<'p0, RS>);

    fn for_each<Func, Ret>(self, mut func: Func)
    where
        Ret: Into<TCF>,
        Func: for<'f, 'p0> FnMut(&'f mut Self::P0<'p0>) -> Ret,
    {
        let mut queue = VecDeque::from([self.start]);

        'queue: while let Some(entity) = queue.pop_front() {
            let Ok(mut curr) = self.control.get_mut(entity) else {
                continue;
            };

            match func(&mut curr).into() {
                TCF::Continue => {}
                TCF::Exit => return,
                TCF::Probe => {
                    queue.clear();
                }
                TCF::Conclude => {
                    continue 'queue;
                }
            }

            queue.extend(Edge::entities(&curr.1));
        }
    }
}

// -----
// Joins
// -----

pub trait JoinForEach<const N: usize> {
    type P0<'p0>;

    fn for_each<Func, Ret>(self, func: Func)
    where
        Ret: Into<JCF>,
        Func: for<'p0> FnMut(Self::P0<'p0>) -> Ret;
}

impl<RS, Edges, Joins, const N: usize> JoinForEach<N>
    for JoinWith<&'_ RelationsItem<'_, RS>, Edges, Joins>
where
    RS: RelationSet,
    Edges: Product<N>,
    Joins: for<'a> Joinable<'a, N>,
    for<'i> RelationsItem<'i, RS>: RelationEntries,
{
    type P0<'p0> = <Joins as Joinable<'p0, N>>::Out;

    fn for_each<Func, Ret>(mut self, mut func: Func)
    where
        Ret: Into<JCF>,
        Func: for<'p0> FnMut(Self::P0<'p0>) -> Ret,
    {
        let mut edge_product = Edges::product(self.relations);
        let mut matches = [false; N];

        while let Some(entities) = edge_product.advance(matches) {
            matches = Joinable::check(&self.items, entities);

            if matches.iter().any(|b| !b) {
                continue;
            }

            match func(Joinable::join(&mut self.items, entities)).into() {
                JCF::Continue => {}
                JCF::Exit => return,
                JCF::FastForward(n) if n < N => {
                    matches[n] = false;
                }
                _ => {}
            }
        }
    }
}
