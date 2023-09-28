use crate::tuple_traits::*;
use crate::{
    for_each::ControlFlow,
    operations::utils::{EdgeSide, Operations, Relations, RelationsItem, SelfTracking},
};

use bevy::ecs::{
    entity::Entity,
    query::{ReadOnlyWorldQuery, WorldQuery},
    system::Query,
};

use std::{borrow::Borrow, collections::VecDeque};

/// A trait to iterate relation queries.
///
/// When iterating:
/// - Diamonds
/// - Cycles
/// - Joins where multiple entities have the same target
///
/// References to the same entities components can be produced more than once which is why this
/// problem cannot be solved with `unsafe`. It requires lending iterators which are not expressable
/// with current GATs so to work around this "lifetime trapping" is used instead.
/// The closure parameters cannot escape the closure.
///
/// For any control query `Query<(X, Relations<..>)>`:
/// - If there is only joins: Permutations of valid entities from the joined queries will be looped
/// through. The left parameter will be the `X` item and the right parameter will be a tuple of all
/// the query fetch parameters from the joined queries.
/// - If there is only hierarchy traversals: Traversable ancestor-descendant permutations that
/// belong to the control query will be looped through. For descents the left parameter will be the
/// `X` item of an ancestor and the right parameter will be the `X` item of an immediate descendant.
/// For ascents this is the other way around.
///
/// See [`ControlFlow`] for control flow options and [`ForEachPermutations3Arity`] for the loop
/// behavior of operations with hierarchy traversals and 1 or more join.
pub trait ForEachPermutations2Arity<const N: usize> {
    type P0<'p0>;
    type P1<'p1>;

    fn for_each<Func, Ret>(self, func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'p0, 'p1> FnMut(&'f mut Self::P0<'p0>, Self::P1<'p1>) -> Ret;
}

// -----
// Joins
// -----

impl<Q, RS, F, JoinedTypes, JoinedQueries, const N: usize> ForEachPermutations2Arity<N>
    for Operations<&'_ Query<'_, '_, (Q, Relations<RS>), F>, JoinedTypes, JoinedQueries, ()>
where
    Q: WorldQuery,
    RS: RelationSet,
    F: ReadOnlyWorldQuery,
    JoinedTypes: Product<N>,
    JoinedQueries: for<'a> Joinable<'a, N>,
    for<'i> RelationsItem<'i, RS>: RelationEntries,
{
    type P0<'p0> = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'p0>;
    type P1<'p1> = <JoinedQueries as Joinable<'p1, N>>::Out;

    fn for_each<Func, Ret>(mut self, mut func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'p0, 'p1> FnMut(&'f mut Self::P0<'p0>, Self::P1<'p1>) -> Ret,
    {
        for (mut control, relations) in self.control.iter() {
            let mut edge_product = JoinedTypes::product(&relations);
            let mut matches = [false; N];

            while let Some(entities) = edge_product.advance(matches) {
                matches = Joinable::check(&self.joined_queries, entities);

                if matches.iter().any(|b| !b) {
                    continue;
                }

                match func(
                    &mut control,
                    Joinable::join(&mut self.joined_queries, entities),
                )
                .into()
                {
                    ControlFlow::Continue => {}
                    ControlFlow::Exit | ControlFlow::Conclude => return,
                    ControlFlow::Walk | ControlFlow::Probe => break,
                    ControlFlow::FastForward(n) if n < N => {
                        matches[n] = false;
                    }
                    _ => {}
                }
            }
        }
    }
}

impl<Q, RS, F, JoinedTypes, JoinedQueries, const N: usize> ForEachPermutations2Arity<N>
    for Operations<&'_ mut Query<'_, '_, (Q, Relations<RS>), F>, JoinedTypes, JoinedQueries, ()>
where
    Q: WorldQuery,
    RS: RelationSet,
    F: ReadOnlyWorldQuery,
    JoinedTypes: Product<N>,
    JoinedQueries: for<'a> Joinable<'a, N>,
    for<'i> RelationsItem<'i, RS>: RelationEntries,
{
    type P0<'p0> = <Q as WorldQuery>::Item<'p0>;
    type P1<'p1> = <JoinedQueries as Joinable<'p1, N>>::Out;

    fn for_each<Func, Ret>(mut self, mut func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'p0, 'p1> FnMut(&'f mut Self::P0<'p0>, Self::P1<'p1>) -> Ret,
    {
        for (mut control, relations) in self.control.iter_mut() {
            let mut edge_product = JoinedTypes::product(&relations);
            let mut matches = [false; N];

            while let Some(entities) = edge_product.advance(matches) {
                matches = Joinable::check(&self.joined_queries, entities);

                if matches.iter().any(|b| !b) {
                    continue;
                }

                match func(
                    &mut control,
                    Joinable::join(&mut self.joined_queries, entities),
                )
                .into()
                {
                    ControlFlow::Continue => {}
                    ControlFlow::Exit | ControlFlow::Conclude => return,
                    ControlFlow::Walk | ControlFlow::Probe => break,
                    ControlFlow::FastForward(n) if n < N => {
                        matches[n] = false;
                    }
                    _ => {}
                }
            }
        }
    }
}

// ------------------------
// - Beadth first traversal
// - Self tracking
// ------------------------

impl<Q, RS, F, T> ForEachPermutations2Arity<0>
    for Operations<&'_ Query<'_, '_, (Q, Relations<RS>), F>, (), (), T, Entity, (), SelfTracking>
where
    Q: WorldQuery,
    RS: RelationSet,
    F: ReadOnlyWorldQuery,
    T: EdgeSide,
    for<'i> RelationsItem<'i, RS>: RelationEntries,
{
    type P0<'p0> = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'p0>;
    type P1<'p1> = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'p1>;

    fn for_each<Func, Ret>(self, mut func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'p0, 'p1> FnMut(&'f mut Self::P0<'p0>, Self::P1<'p1>) -> Ret,
    {
        let mut queue = VecDeque::from([self.start]);

        'queue: while let Some(entity) = queue.pop_front() {
            let Ok((mut control, relations)) = self.control.get(entity) else {
                continue;
            };

            for e in T::entities(&relations) {
                let Ok(traversal_item) = self.control.get(e) else {
                    continue;
                };

                match func(&mut control, traversal_item.0).into() {
                    ControlFlow::Exit => return,
                    ControlFlow::Conclude => {
                        continue 'queue;
                    }
                    ControlFlow::Probe => {
                        queue.clear();
                        queue.push_back(e);
                        continue 'queue;
                    }
                    _ => {}
                }
            }

            queue.extend(T::entities(&relations));
        }
    }
}

impl<Q, RS, F, T> ForEachPermutations2Arity<0>
    for Operations<
        &'_ mut Query<'_, '_, (Q, Relations<RS>), F>,
        (),
        (),
        T,
        Entity,
        (),
        SelfTracking,
    >
where
    Q: WorldQuery,
    RS: RelationSet,
    F: ReadOnlyWorldQuery,
    T: EdgeSide,
    for<'i> RelationsItem<'i, RS>: RelationEntries,
{
    type P0<'p0> = <Q as WorldQuery>::Item<'p0>;
    type P1<'p1> = <Q as WorldQuery>::Item<'p1>;

    fn for_each<Func, Ret>(self, mut func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'p0, 'p1> FnMut(&'f mut Self::P0<'p0>, Self::P1<'p1>) -> Ret,
    {
        let mut queue = VecDeque::from([self.start]);

        'queue: while let Some(entity) = queue.pop_front() {
            // SAFETY: Self referential relations are impossible so this is always safe.
            let Ok((mut control, relations)) = (unsafe { self.control.get_unchecked(entity) })
            else {
                continue;
            };

            for e in T::entities(&relations) {
                // SAFETY: Self referential relations are impossible so this is always safe.
                let Ok(traversal_item) = (unsafe { self.control.get_unchecked(e) }) else {
                    continue;
                };

                match func(&mut control, traversal_item.0).into() {
                    ControlFlow::Exit => return,
                    ControlFlow::Conclude => {
                        continue 'queue;
                    }
                    ControlFlow::Probe => {
                        queue.clear();
                        queue.push_back(e);
                        continue 'queue;
                    }
                    _ => {}
                }
            }

            queue.extend(T::entities(&relations));
        }
    }
}
