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

// ----------
// Traversals
// ----------
pub trait TraversalForEach<RS: RelationSet> {
    type WQ<'wq>;

    fn for_each<Func, Ret>(self, func: Func)
    where
        Ret: Into<TCF>,
        Func: for<'f, 'i> FnMut((&'f mut Self::WQ<'i>, &'f RelationsItem<'i, RS>)) -> Ret;
}

impl<Q, RS, F, Edge, E, Starts> TraversalForEach<RS>
    for TraverseAnd<&'_ Query<'_, '_, (Q, Relations<RS>), F>, Edge, Starts>
where
    Q: WorldQuery,
    RS: RelationSet,
    F: ReadOnlyWorldQuery,
    Edge: EdgeSide,
    E: Borrow<Entity>,
    Starts: IntoIterator<Item = E>,
    for<'i> RelationsItem<'i, RS>: RelationEntries,
{
    type WQ<'wq> = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'wq>;

    fn for_each<Func, Ret>(self, mut func: Func)
    where
        Ret: Into<TCF>,
        Func: for<'f, 'i> FnMut((&'f mut Self::WQ<'i>, &'f RelationsItem<'i, RS>)) -> Ret,
    {
        let mut queue = self
            .starts
            .into_iter()
            .map(|e| *e.borrow())
            .collect::<VecDeque<Entity>>();

        while let Some(entity) = queue.pop_front() {
            let Ok((mut wq, edges)) = self.control.get(entity) else {
                continue;
            };

            match func((&mut wq, &edges)).into() {
                TCF::Continue => {}
                TCF::Exit => return,
                TCF::Probe => {
                    queue.clear();
                }
                TCF::Conclude => {
                    continue;
                }
            }

            queue.extend(Edge::entities(&edges));
        }
    }
}

impl<Q, RS, F, Edge, E, Starts> TraversalForEach<RS>
    for TraverseAnd<&'_ mut Query<'_, '_, (Q, Relations<RS>), F>, Edge, Starts>
where
    Q: WorldQuery,
    RS: RelationSet,
    F: ReadOnlyWorldQuery,
    Edge: EdgeSide,
    E: Borrow<Entity>,
    Starts: IntoIterator<Item = E>,
    for<'i> RelationsItem<'i, RS>: RelationEntries,
{
    type WQ<'wq> = <Q as WorldQuery>::Item<'wq>;

    fn for_each<Func, Ret>(self, mut func: Func)
    where
        Ret: Into<TCF>,
        Func: for<'f, 'i> FnMut((&'f mut Self::WQ<'i>, &'f RelationsItem<'i, RS>)) -> Ret,
    {
        let mut queue = self
            .starts
            .into_iter()
            .map(|e| *e.borrow())
            .collect::<VecDeque<Entity>>();

        while let Some(entity) = queue.pop_front() {
            let Ok((mut wq, edges)) = self.control.get_mut(entity) else {
                continue;
            };

            match func((&mut wq, &edges)).into() {
                TCF::Continue => {}
                TCF::Exit => return,
                TCF::Probe => {
                    queue.clear();
                }
                TCF::Conclude => {
                    continue;
                }
            }

            queue.extend(Edge::entities(&edges));
        }
    }
}

// -----------------
// - Traversal
// - Remote tracking
// -----------------
pub trait RemoteTrackingTraversalForEach<RS: RelationSet, const N: usize> {
    type WQ<'wq>;
    type Tracked<'t>;

    fn for_each<Func, Ret>(self, func: Func)
    where
        Ret: Into<TCF>,
        Func: for<'f, 'l, 'r> FnMut(
            Self::Tracked<'l>,
            (&'f mut Self::WQ<'r>, &'f RelationsItem<'r, RS>),
        ) -> Ret;
}

impl<Q, RS, F, Edge, E, Starts, Tracked, const N: usize> RemoteTrackingTraversalForEach<RS, N>
    for TraverseAnd<&'_ Query<'_, '_, (Q, Relations<RS>), F>, Edge, Starts, Tracked>
where
    Q: WorldQuery,
    RS: RelationSet,
    F: ReadOnlyWorldQuery,
    Edge: EdgeSide,
    E: Borrow<Entity>,
    Starts: IntoIterator<Item = E>,
    for<'i> RelationsItem<'i, RS>: RelationEntries,
    Tracked: for<'a> Trackable<'a, N>,
{
    type WQ<'wq> = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'wq>;
    type Tracked<'t> = <Tracked as Trackable<'t, N>>::Out;

    fn for_each<Func, Ret>(mut self, mut func: Func)
    where
        Ret: Into<TCF>,
        Func: for<'f, 'l, 'r> FnMut(
            Self::Tracked<'l>,
            (&'f mut Self::WQ<'r>, &'f RelationsItem<'r, RS>),
        ) -> Ret,
    {
        let mut queue = self
            .starts
            .into_iter()
            .map(|e| *e.borrow())
            .map(|e| (e, [e; N]))
            .collect::<VecDeque<_>>();

        while let Some((entity, last)) = queue.pop_front() {
            let Ok((mut wq, edges)) = self.control.get(entity) else {
                continue;
            };

            if let Some(retrieved) = Trackable::retrieve(&mut self.track, last) {
                match func(retrieved, (&mut wq, &edges)).into() {
                    TCF::Continue => {}
                    TCF::Exit => return,
                    TCF::Conclude => {
                        continue;
                    }
                    TCF::Probe => {
                        queue.clear();
                    }
                }
            }

            for e in Edge::entities(&edges) {
                queue.push_back((e, Trackable::update(&self.track, e, last)));
            }
        }
    }
}

impl<Q, RS, F, Edge, E, Starts, Tracked, const N: usize> RemoteTrackingTraversalForEach<RS, N>
    for TraverseAnd<&'_ mut Query<'_, '_, (Q, Relations<RS>), F>, Edge, Starts, Tracked>
where
    Q: WorldQuery,
    RS: RelationSet,
    F: ReadOnlyWorldQuery,
    Edge: EdgeSide,
    E: Borrow<Entity>,
    Starts: IntoIterator<Item = E>,
    for<'i> RelationsItem<'i, RS>: RelationEntries,
    Tracked: for<'a> Trackable<'a, N>,
{
    type WQ<'wq> = <Q as WorldQuery>::Item<'wq>;
    type Tracked<'t> = <Tracked as Trackable<'t, N>>::Out;

    fn for_each<Func, Ret>(mut self, mut func: Func)
    where
        Ret: Into<TCF>,
        Func: for<'f, 'l, 'r> FnMut(
            Self::Tracked<'l>,
            (&'f mut Self::WQ<'r>, &'f RelationsItem<'r, RS>),
        ) -> Ret,
    {
        let mut queue = self
            .starts
            .into_iter()
            .map(|e| *e.borrow())
            .map(|e| (e, [e; N]))
            .collect::<VecDeque<_>>();

        while let Some((entity, last)) = queue.pop_front() {
            let Ok((mut wq, edges)) = self.control.get_mut(entity) else {
                continue;
            };

            if let Some(retrieved) = Trackable::retrieve(&mut self.track, last) {
                match func(retrieved, (&mut wq, &edges)).into() {
                    TCF::Continue => {}
                    TCF::Exit => return,
                    TCF::Conclude => {
                        continue;
                    }
                    TCF::Probe => {
                        queue.clear();
                    }
                }
            }

            for e in Edge::entities(&edges) {
                queue.push_back((e, Trackable::update(&self.track, e, last)));
            }
        }
    }
}

// ---------------
// - Traversal
// - Self tracking
// ---------------
pub trait SelfTrackingTraversalForEach<RS: RelationSet> {
    type WQ<'wq>;

    fn for_each<Func, Ret>(self, func: Func)
    where
        Ret: Into<TCF>,
        Func: for<'f, 'l, 'r> FnMut(
            (&'f mut Self::WQ<'l>, &'f RelationsItem<'l, RS>),
            (&'f mut Self::WQ<'r>, &'f RelationsItem<'r, RS>),
        ) -> Ret;
}

impl<Q, RS, F, Edge, E, Starts> SelfTrackingTraversalForEach<RS>
    for TraverseAnd<&'_ Query<'_, '_, (Q, Relations<RS>), F>, Edge, Starts, (), true>
where
    Q: WorldQuery,
    RS: RelationSet,
    F: ReadOnlyWorldQuery,
    Edge: EdgeSide,
    E: Borrow<Entity>,
    Starts: IntoIterator<Item = E>,
    for<'i> RelationsItem<'i, RS>: RelationEntries,
{
    type WQ<'wq> = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'wq>;

    fn for_each<Func, Ret>(self, mut func: Func)
    where
        Ret: Into<TCF>,
        Func: for<'f, 'l, 'r> FnMut(
            (&'f mut Self::WQ<'l>, &'f RelationsItem<'l, RS>),
            (&'f mut Self::WQ<'r>, &'f RelationsItem<'r, RS>),
        ) -> Ret,
    {
        let mut queue = self
            .starts
            .into_iter()
            .map(|e| *e.borrow())
            .collect::<VecDeque<Entity>>();

        'queue: while let Some(entity) = queue.pop_front() {
            let Ok((mut left_wq, left_edges)) = self.control.get(entity) else {
                continue;
            };

            for e in Edge::entities(&left_edges) {
                let Ok((mut right_wq, right_edges)) = self.control.get(e) else {
                    continue;
                };

                match func((&mut left_wq, &left_edges), (&mut right_wq, &right_edges)).into() {
                    TCF::Continue => {}
                    TCF::Exit => return,
                    TCF::Conclude => {
                        continue 'queue;
                    }
                    TCF::Probe => {
                        queue.clear();
                        queue.push_back(e);
                        continue 'queue;
                    }
                }
            }

            queue.extend(Edge::entities(&left_edges));
        }
    }
}

impl<Q, RS, F, Edge, E, Starts> SelfTrackingTraversalForEach<RS>
    for TraverseAnd<&'_ mut Query<'_, '_, (Q, Relations<RS>), F>, Edge, Starts, (), true>
where
    Q: WorldQuery,
    RS: RelationSet,
    F: ReadOnlyWorldQuery,
    Edge: EdgeSide,
    E: Borrow<Entity>,
    Starts: IntoIterator<Item = E>,
    for<'i> RelationsItem<'i, RS>: RelationEntries,
{
    type WQ<'wq> = <Q as WorldQuery>::Item<'wq>;

    fn for_each<Func, Ret>(self, mut func: Func)
    where
        Ret: Into<TCF>,
        Func: for<'f, 'l, 'r> FnMut(
            (&'f mut Self::WQ<'l>, &'f RelationsItem<'l, RS>),
            (&'f mut Self::WQ<'r>, &'f RelationsItem<'r, RS>),
        ) -> Ret,
    {
        let mut queue = self
            .starts
            .into_iter()
            .map(|e| *e.borrow())
            .collect::<VecDeque<Entity>>();

        'queue: while let Some(entity) = queue.pop_front() {
            // SAFETY: Self referential relations are impossible so this is always safe
            let Ok((mut left_wq, left_edges)) = (unsafe { self.control.get_unchecked(entity) })
            else {
                continue;
            };

            for e in Edge::entities(&left_edges) {
                // SAFETY: Self referential relations are impossible so this is always safe
                let Ok((mut right_wq, right_edges)) = (unsafe { self.control.get_unchecked(e) })
                else {
                    continue;
                };

                match func((&mut left_wq, &left_edges), (&mut right_wq, &right_edges)).into() {
                    TCF::Continue => {}
                    TCF::Exit => return,
                    TCF::Conclude => {
                        continue 'queue;
                    }
                    TCF::Probe => {
                        queue.clear();
                        queue.push_back(e);
                        continue 'queue;
                    }
                }
            }

            queue.extend(Edge::entities(&left_edges));
        }
    }
}

// -----------------
// - Traversal
// - Remote tracking
// - Self tracking
// -----------------
pub trait RemoteSelfTrackingTraversalForEach<RS: RelationSet, const N: usize> {
    type WQ<'wq>;
    type Tracked<'t>;

    fn for_each<Func, Ret>(self, func: Func)
    where
        Ret: Into<TCF>,
        Func: for<'f, 'l, 'r> FnMut(
            Self::Tracked<'l>,
            (&'f mut Self::WQ<'r>, &'f RelationsItem<'r, RS>),
            (&'f mut Self::WQ<'r>, &'f RelationsItem<'r, RS>),
        ) -> Ret;
}

impl<Q, RS, F, Edge, E, Starts, Tracked, const N: usize> RemoteSelfTrackingTraversalForEach<RS, N>
    for TraverseAnd<&'_ Query<'_, '_, (Q, Relations<RS>), F>, Edge, Starts, Tracked, true>
where
    Q: WorldQuery,
    RS: RelationSet,
    F: ReadOnlyWorldQuery,
    Edge: EdgeSide,
    E: Borrow<Entity>,
    Starts: IntoIterator<Item = E>,
    for<'i> RelationsItem<'i, RS>: RelationEntries,
    Tracked: for<'a> Trackable<'a, N>,
{
    type WQ<'wq> = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'wq>;
    type Tracked<'t> = <Tracked as Trackable<'t, N>>::Out;

    fn for_each<Func, Ret>(mut self, mut func: Func)
    where
        Ret: Into<TCF>,
        Func: for<'f, 'l, 'r> FnMut(
            Self::Tracked<'l>,
            (&'f mut Self::WQ<'r>, &'f RelationsItem<'r, RS>),
            (&'f mut Self::WQ<'r>, &'f RelationsItem<'r, RS>),
        ) -> Ret,
    {
        let mut queue = self
            .starts
            .into_iter()
            .map(|e| *e.borrow())
            .map(|e| (e, [e; N]))
            .collect::<VecDeque<_>>();

        'queue: while let Some((entity, last)) = queue.pop_front() {
            let Ok((mut left_wq, left_edges)) = self.control.get(entity) else {
                continue;
            };

            for e in Edge::entities(&left_edges) {
                let Ok((mut right_wq, right_edges)) = self.control.get(e) else {
                    continue;
                };

                let updated = Trackable::update(&self.track, e, last);

                if let Some(retrieved) = Trackable::retrieve(&mut self.track, updated) {
                    match func(
                        retrieved,
                        (&mut left_wq, &left_edges),
                        (&mut right_wq, &right_edges),
                    )
                    .into()
                    {
                        TCF::Continue => {}
                        TCF::Exit => return,
                        TCF::Conclude => {
                            continue 'queue;
                        }
                        TCF::Probe => {
                            queue.clear();
                            queue.push_back((e, updated));
                            continue 'queue;
                        }
                    }
                }

                queue.push_back((e, updated));
            }
        }
    }
}

impl<Q, RS, F, Edge, E, Starts, Tracked, const N: usize> RemoteSelfTrackingTraversalForEach<RS, N>
    for TraverseAnd<&'_ mut Query<'_, '_, (Q, Relations<RS>), F>, Edge, Starts, Tracked, true>
where
    Q: WorldQuery,
    RS: RelationSet,
    F: ReadOnlyWorldQuery,
    Edge: EdgeSide,
    E: Borrow<Entity>,
    Starts: IntoIterator<Item = E>,
    for<'i> RelationsItem<'i, RS>: RelationEntries,
    Tracked: for<'a> Trackable<'a, N>,
{
    type WQ<'wq> = <Q as WorldQuery>::Item<'wq>;
    type Tracked<'t> = <Tracked as Trackable<'t, N>>::Out;

    fn for_each<Func, Ret>(mut self, mut func: Func)
    where
        Ret: Into<TCF>,
        Func: for<'f, 'l, 'r> FnMut(
            Self::Tracked<'l>,
            (&'f mut Self::WQ<'r>, &'f RelationsItem<'r, RS>),
            (&'f mut Self::WQ<'r>, &'f RelationsItem<'r, RS>),
        ) -> Ret,
    {
        let mut queue = self
            .starts
            .into_iter()
            .map(|e| *e.borrow())
            .map(|e| (e, [e; N]))
            .collect::<VecDeque<_>>();

        'queue: while let Some((entity, last)) = queue.pop_front() {
            // SAFETY: Self referential relations are impossible so this is always safe
            let Ok((mut left_wq, left_edges)) = (unsafe { self.control.get_unchecked(entity) })
            else {
                continue;
            };

            for e in Edge::entities(&left_edges) {
                // SAFETY: Self referential relations are impossible so this is always safe
                let Ok((mut right_wq, right_edges)) = (unsafe { self.control.get_unchecked(e) })
                else {
                    continue;
                };

                let updated = Trackable::update(&self.track, e, last);

                if let Some(retrieved) = Trackable::retrieve(&mut self.track, updated) {
                    match func(
                        retrieved,
                        (&mut left_wq, &left_edges),
                        (&mut right_wq, &right_edges),
                    )
                    .into()
                    {
                        TCF::Continue => {}
                        TCF::Exit => return,
                        TCF::Conclude => {
                            continue 'queue;
                        }
                        TCF::Probe => {
                            queue.clear();
                            queue.push_back((e, updated));
                            continue 'queue;
                        }
                    }
                }

                queue.push_back((e, updated));
            }
        }
    }
}
