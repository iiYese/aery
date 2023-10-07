use crate::operations::utils::{
    EdgeSide, JoinWith, Relations, RelationsItem, SelfTracking, TraverseAnd,
};
use crate::tuple_traits::*;

use bevy::ecs::{
    entity::Entity,
    query::{ReadOnlyWorldQuery, WorldQuery},
    system::Query,
};

use std::{borrow::Borrow, collections::VecDeque};

/// Control flow enum for [`Join`] operations.
///
/// [`Join`]: crate::operations::Join
pub enum JCF {
    /// Continue iterating permutations of matches.
    Continue,
    /// Skip permutations until the nth join is advanced to the next match. Useful for multiple
    /// joins to avoid unnecessary permutation iteration. Fast forwarding is an `O(1)` operation.
    /// ## FastForward Illustration:
    /// ```
    /// use bevy::prelude::*;
    /// use aery::prelude::*;
    ///
    /// #[derive(Component)]
    /// struct A;
    ///
    /// #[derive(Component)]
    /// struct B(usize);
    ///
    /// #[derive(Component)]
    /// struct C(usize);
    ///
    /// #[derive(Component)]
    /// struct D(usize);
    ///
    /// #[derive(Relation)]
    /// struct R0;
    ///
    /// #[derive(Relation)]
    /// struct R1;
    ///
    /// #[derive(Relation)]
    /// struct R2;
    ///
    /// fn setup(world: &mut World) {
    ///     let a = world.spawn(A).id();
    ///
    ///     for n in 0..3 {
    ///         world.spawn(B(n)).set::<R0>(a);
    ///         world.spawn(C(n)).set::<R1>(a);
    ///         world.spawn(D(n)).set::<R2>(a);
    ///     }
    /// }
    ///
    /// fn fast_forward(
    ///     a: Query<(&A, Relations<(R0, R1, R2)>)>,
    ///     b: Query<&B>,
    ///     c: Query<&C>,
    ///     d: Query<&D>
    /// ) {
    ///     for (_, edges) in a.iter() {
    ///         edges
    ///             .join::<R0>(&b)
    ///             .join::<R1>(&c)
    ///             .join::<R2>(&d)
    ///             .for_each(|(b, c, d)| {
    ///                 if c.0 == 1 { return JCF::FastForward(1) }
    ///                 println!("({}, {}, {})", b.0, c.0, d.0);
    ///                 JCF::Continue
    ///             });
    ///     }
    /// }
    /// ```
    /// Output:
    /// ```ignore
    ///     (0, 0, 0)
    ///     (0, 0, 1)
    ///     (0, 0, 2)
    /// //  Skipped:
    /// //  (0, 1, 0)
    /// //  (0, 1, 1)
    /// //  (0, 1, 2)
    ///     (0, 2, 0)
    ///     (0, 2, 1)
    ///     (0, 2, 2)
    ///     (1, 0, 0)
    ///     (1, 0, 1)
    ///     (1, 0, 2)
    /// //  Skipped:
    /// //  (1, 1, 0)
    /// //  (1, 1, 1)
    /// //  (1, 1, 2)
    ///     (1, 2, 0)
    ///     (1, 2, 1)
    ///     (1, 2, 2)
    ///     (2, 0, 0)
    ///     (2, 0, 1)
    ///     (2, 0, 2)
    /// //  Skipped:
    /// //  (2, 1, 0)
    /// //  (2, 1, 1)
    /// //  (2, 1, 2)
    ///     (2, 2, 0)
    ///     (2, 2, 1)
    ///     (2, 2, 2)
    /// ```
    FastForward(usize),
    /// Terminate the loop.
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
#[allow(missing_docs)]
pub trait JoinForEach<const N: usize> {
    type Joined<'j>;

    fn for_each<Func, Ret>(self, func: Func)
    where
        Ret: Into<JCF>,
        Func: for<'j> FnMut(Self::Joined<'j>) -> Ret;
}

impl<RS, Edges, Joins, const N: usize> JoinForEach<N>
    for JoinWith<&'_ RelationsItem<'_, RS>, Edges, Joins>
where
    RS: RelationSet,
    Edges: Product<N>,
    Joins: for<'a> Joinable<'a, N>,
    for<'i> RelationsItem<'i, RS>: RelationEntries,
{
    type Joined<'j> = <Joins as Joinable<'j, N>>::Out;

    fn for_each<Func, Ret>(mut self, mut func: Func)
    where
        Ret: Into<JCF>,
        Func: for<'j> FnMut(Self::Joined<'j>) -> Ret,
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

#[cfg_attr(doc, aquamarine::aquamarine)]
/// Control flow enum for [`Traverse`] operations.
/// ## Illustrations
/// Pink == traversed.
///
/// ### Close
/// ```rust
///# use bevy::prelude::*;
///# use aery::prelude::*;
///# #[derive(Relation)]
///# struct R;
///#
///# #[derive(Component)]
///# struct E(usize);
///
/// fn sys(query: Query<(&E, Relations<R>)>, roots: Query<Entity, Root<R>>) {
///     query.traverse::<R>(roots.iter()).for_each(|E(e), _| {
///         if *e == 2 { return TCF::Close }
///
///         // do stuff
///
///         TCF::Continue
///     })
/// }
/// ```
/// ```mermaid
/// flowchart BT
/// classDef pink fill:#f66
///
/// E1:::pink --R--> E0:::pink
/// E2:::pink --R--> E0:::pink
/// E3:::pink --R--> E0:::pink
///
/// E4:::pink --R--> E1:::pink
/// E5:::pink --R--> E1:::pink
///
/// E6 --R--> E2:::pink
/// E7 --R--> E2:::pink
///
/// E8:::pink --R--> E3
/// E9:::pink --R--> E3
/// ```
///
/// ### Probe
/// ```rust
///# use bevy::prelude::*;
///# use aery::prelude::*;
///# #[derive(Relation)]
///# struct R;
///#
///# #[derive(Component)]
///# struct E(usize);
///
/// fn sys(query: Query<(&E, Relations<R>)>, roots: Query<Entity, Root<R>>) {
///     query.traverse::<R>(roots.iter()).for_each(|E(e), _| {
///         if *e == 2 { return TCF::Probe }
///
///         // do stuff
///
///         TCF::Continue
///     })
/// }
/// ```
/// ```mermaid
/// flowchart BT
/// classDef pink fill:#f66
///
/// E1:::pink --R--> E0:::pink
/// E2:::pink --R--> E0:::pink
/// E3 --R--> E0:::pink
///
/// E4 --R--> E1:::pink
/// E5 --R--> E1:::pink
///
/// E6:::pink --R--> E2:::pink
/// E7:::pink --R--> E2:::pink
///
/// E8 --R--> E3
/// E9 --R--> E3
/// ```
///
/// [`Traverse`]: crate::operations::Traverse
pub enum TCF {
    /// Continue going through the graph.
    Continue,
    /// Close the current node off from expansion. Traversal will not go deeper into the graph
    /// through this node.
    Close,
    /// Immediately expand the current node's edges & close all other open paths for further
    /// expansion.
    Probe,
    /// Terminate the loop.
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
#[allow(missing_docs)]
pub trait TraversalForEach<RS: RelationSet> {
    type WQ<'wq>;

    fn for_each<Func, Ret>(self, func: Func)
    where
        Ret: Into<TCF>,
        Func: for<'a> FnMut(&mut Self::WQ<'a>, &RelationsItem<'a, RS>) -> Ret;
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
        Func: for<'a> FnMut(&mut Self::WQ<'a>, &RelationsItem<'a, RS>) -> Ret,
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

            match func(&mut wq, &edges).into() {
                TCF::Continue => {}
                TCF::Exit => return,
                TCF::Probe => {
                    queue.clear();
                }
                TCF::Close => {
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
        Func: for<'a> FnMut(&mut Self::WQ<'a>, &RelationsItem<'a, RS>) -> Ret,
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

            match func(&mut wq, &edges).into() {
                TCF::Continue => {}
                TCF::Exit => return,
                TCF::Probe => {
                    queue.clear();
                }
                TCF::Close => {
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
#[allow(missing_docs)]
pub trait RemoteTrackingTraversalForEach<RS: RelationSet, const N: usize> {
    type WQ<'wq>;
    type Tracked<'t>;

    fn for_each<Func, Ret>(self, func: Func)
    where
        Ret: Into<TCF>,
        Func:
            for<'t, 'a> FnMut(Self::Tracked<'t>, &mut Self::WQ<'a>, &RelationsItem<'a, RS>) -> Ret;
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
        Func:
            for<'t, 'a> FnMut(Self::Tracked<'t>, &mut Self::WQ<'a>, &RelationsItem<'a, RS>) -> Ret,
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
                match func(retrieved, &mut wq, &edges).into() {
                    TCF::Continue => {}
                    TCF::Exit => return,
                    TCF::Close => {
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
        Func:
            for<'t, 'a> FnMut(Self::Tracked<'t>, &mut Self::WQ<'a>, &RelationsItem<'a, RS>) -> Ret,
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
                match func(retrieved, &mut wq, &edges).into() {
                    TCF::Continue => {}
                    TCF::Exit => return,
                    TCF::Close => {
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
#[allow(missing_docs)]
pub trait SelfTrackingTraversalForEach<RS: RelationSet> {
    type WQ<'wq>;

    fn for_each<Func, Ret>(self, func: Func)
    where
        Ret: Into<TCF>,
        Func: for<'a> FnMut(
            &mut Self::WQ<'a>,
            &RelationsItem<'a, RS>,
            &mut Self::WQ<'a>,
            &RelationsItem<'a, RS>,
        ) -> Ret;
}

impl<Q, RS, F, Edge, E, Starts> SelfTrackingTraversalForEach<RS>
    for TraverseAnd<&'_ Query<'_, '_, (Q, Relations<RS>), F>, Edge, Starts, SelfTracking>
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
        Func: for<'a> FnMut(
            &mut Self::WQ<'a>,
            &RelationsItem<'a, RS>,
            &mut Self::WQ<'a>,
            &RelationsItem<'a, RS>,
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

                match func(&mut left_wq, &left_edges, &mut right_wq, &right_edges).into() {
                    TCF::Continue => {}
                    TCF::Exit => return,
                    TCF::Close => {
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
    for TraverseAnd<&'_ mut Query<'_, '_, (Q, Relations<RS>), F>, Edge, Starts, SelfTracking>
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
        Func: for<'a> FnMut(
            &mut Self::WQ<'a>,
            &RelationsItem<'a, RS>,
            &mut Self::WQ<'a>,
            &RelationsItem<'a, RS>,
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

                match func(&mut left_wq, &left_edges, &mut right_wq, &right_edges).into() {
                    TCF::Continue => {}
                    TCF::Exit => return,
                    TCF::Close => {
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

// ---------------
// - Traversal
// - Self tracking
// - Fold breadth
// ---------------
#[allow(missing_docs)]
pub trait FoldingSelfTrackingTraversalForEach<RS: RelationSet> {
    type WQ<'wq>;
    type Res;

    fn for_each<Func, Ret>(self, func: Func)
    where
        Ret: Into<TCF>,
        Func: for<'a> FnMut(
            &mut Self::Res,
            &mut Self::WQ<'a>,
            &RelationsItem<'a, RS>,
            &mut Self::WQ<'a>,
            &RelationsItem<'a, RS>,
        ) -> Ret;
}

impl<Q, RS, F, Edge, E, Starts, Init, Fold, Acc, Err> FoldingSelfTrackingTraversalForEach<RS>
    for TraverseAnd<
        &'_ Query<'_, '_, (Q, Relations<RS>), F>,
        Edge,
        Starts,
        SelfTracking,
        Init,
        Fold,
    >
where
    Q: WorldQuery,
    RS: RelationSet,
    F: ReadOnlyWorldQuery,
    Edge: EdgeSide,
    E: Borrow<Entity>,
    Starts: IntoIterator<Item = E>,
    for<'i> RelationsItem<'i, RS>: RelationEntries,
    Init: for<'a> FnMut(
        &mut <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'a>,
        &RelationsItem<'a, RS>,
    ) -> Acc,
    Fold: for<'a> FnMut(
        Acc,
        &mut <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'a>,
        &RelationsItem<'a, RS>,
    ) -> Result<Acc, Err>,
{
    type WQ<'wq> = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'wq>;
    type Res = Result<Acc, Err>;

    fn for_each<Func, Ret>(mut self, mut func: Func)
    where
        Ret: Into<TCF>,
        Func: for<'a> FnMut(
            &mut Self::Res,
            &mut Self::WQ<'a>,
            &RelationsItem<'a, RS>,
            &mut Self::WQ<'a>,
            &RelationsItem<'a, RS>,
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

            let mut acc = Ok::<_, Err>((self.init)(&mut left_wq, &left_edges));

            for e in Edge::entities(&left_edges) {
                let Ok((mut item, edges)) = self.control.get(e) else {
                    continue;
                };

                let Ok(acc_ok) = acc else {
                    break;
                };

                acc = (self.fold)(acc_ok, &mut item, &edges);
            }

            for e in Edge::entities(&left_edges) {
                let Ok((mut right_wq, right_edges)) = self.control.get(e) else {
                    continue;
                };

                match func(
                    &mut acc,
                    &mut left_wq,
                    &left_edges,
                    &mut right_wq,
                    &right_edges,
                )
                .into()
                {
                    TCF::Continue => {}
                    TCF::Exit => return,
                    TCF::Close => {
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

impl<Q, RS, F, Edge, E, Starts, Init, Fold, Acc, Err> FoldingSelfTrackingTraversalForEach<RS>
    for TraverseAnd<
        &'_ mut Query<'_, '_, (Q, Relations<RS>), F>,
        Edge,
        Starts,
        SelfTracking,
        Init,
        Fold,
    >
where
    Q: WorldQuery,
    RS: RelationSet,
    F: ReadOnlyWorldQuery,
    Edge: EdgeSide,
    E: Borrow<Entity>,
    Starts: IntoIterator<Item = E>,
    for<'i> RelationsItem<'i, RS>: RelationEntries,
    Init: for<'a> FnMut(&mut <Q as WorldQuery>::Item<'a>, &RelationsItem<'a, RS>) -> Acc,
    Fold: for<'a> FnMut(
        Acc,
        &mut <Q as WorldQuery>::Item<'a>,
        &RelationsItem<'a, RS>,
    ) -> Result<Acc, Err>,
{
    type WQ<'wq> = <Q as WorldQuery>::Item<'wq>;
    type Res = Result<Acc, Err>;

    fn for_each<Func, Ret>(mut self, mut func: Func)
    where
        Ret: Into<TCF>,
        Func: for<'a> FnMut(
            &mut Self::Res,
            &mut Self::WQ<'a>,
            &RelationsItem<'a, RS>,
            &mut Self::WQ<'a>,
            &RelationsItem<'a, RS>,
        ) -> Ret,
    {
        let mut queue = self
            .starts
            .into_iter()
            .map(|e| *e.borrow())
            .collect::<VecDeque<Entity>>();

        'queue: while let Some(entity) = queue.pop_front() {
            // SAFETY: Self referential relations are impossible so this is always safe.
            let Ok((mut left_wq, left_edges)) = (unsafe { self.control.get_unchecked(entity) })
            else {
                continue;
            };

            let mut acc = Ok::<_, Err>((self.init)(&mut left_wq, &left_edges));

            for e in Edge::entities(&left_edges) {
                // SAFETY: Self referential relations are impossible so this is always safe.
                let Ok((mut item, edges)) = (unsafe { self.control.get_unchecked(e) }) else {
                    continue;
                };

                let Ok(acc_ok) = acc else {
                    break;
                };

                acc = (self.fold)(acc_ok, &mut item, &edges);
            }

            for e in Edge::entities(&left_edges) {
                // SAFETY: Self referential relations are impossible so this is always safe.
                let Ok((mut right_wq, right_edges)) = (unsafe { self.control.get_unchecked(e) })
                else {
                    continue;
                };

                match func(
                    &mut acc,
                    &mut left_wq,
                    &left_edges,
                    &mut right_wq,
                    &right_edges,
                )
                .into()
                {
                    TCF::Continue => {}
                    TCF::Exit => return,
                    TCF::Close => {
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
