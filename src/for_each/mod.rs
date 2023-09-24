use crate::tuple_traits::*;

use bevy::ecs::{
    entity::Entity,
    query::{ReadOnlyWorldQuery, WorldQuery},
    system::Query,
};

use std::{borrow::Borrow, collections::VecDeque};

use super::{ControlFlow, EdgeSide, Operations, Relations};

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
pub trait ForEachPermutations<const N: usize> {
    type P0<'p0>;
    type P1<'p1>;

    fn for_each<Func, Ret>(self, func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'p0, 'p1> FnMut(&'f mut Self::P0<'p0>, Self::P1<'p1>) -> Ret;
}

/*impl<Q, R, F, JoinedTypes, JoinedQueries, const N: usize> ForEachPermutations<N>
    for Operations<&'_ Query<'_, '_, (Q, Relations<R>), F>, JoinedTypes, JoinedQueries, ()>
where
    Q: WorldQuery,
    R: RelationSet,
    F: ReadOnlyWorldQuery,
    JoinedTypes: Product<N, R>,
    JoinedQueries: for<'a> Joinable<'a, N>,
{
    type P0<'p0> = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'p0>;
    type P1<'p1> = <JoinedQueries as Joinable<'p1, N>>::Out;

    fn for_each<Func, Ret>(mut self, mut func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'p0, 'p1> FnMut(&'f mut Self::P0<'p0>, Self::P1<'p1>) -> Ret,
    {
        for (mut control, relations) in self.control.iter() {
            let mut edge_product = JoinedTypes::product(&relations.edges);
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

impl<Q, R, F, JoinedTypes, JoinedQueries, const N: usize> ForEachPermutations<N>
    for Operations<&'_ mut Query<'_, '_, (Q, Relations<R>), F>, JoinedTypes, JoinedQueries, ()>
where
    Q: WorldQuery,
    R: RelationSet,
    F: ReadOnlyWorldQuery,
    JoinedTypes: Product<N, R>,
    JoinedQueries: for<'a> Joinable<'a, N>,
{
    type P0<'p0> = <Q as WorldQuery>::Item<'p0>;
    type P1<'p1> = <JoinedQueries as Joinable<'p1, N>>::Out;

    fn for_each<Func, Ret>(mut self, mut func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'p0, 'p1> FnMut(&'f mut Self::P0<'p0>, Self::P1<'p1>) -> Ret,
    {
        for (mut control, relations) in self.control.iter_mut() {
            let mut edge_product = JoinedTypes::product(&relations.edges);
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
}*/

impl<Q, RS, F, T, E, I> ForEachPermutations<0>
    for Operations<&'_ Query<'_, '_, (Q, Relations<RS>), F>, (), (), T, I>
where
    Q: WorldQuery,
    RS: RelationSet,

    RS::Edges: PadMax,
    <RS::Edges as PadMax>::Padded: ReadOnlyWorldQuery,
    F: ReadOnlyWorldQuery,
    T: EdgeSide,
    E: Borrow<Entity>,
    I: IntoIterator<Item = E>,
{
    type P0<'p0> = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'p0>;
    type P1<'p1> = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'p1>;

    fn for_each<Func, Ret>(self, mut func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'p0, 'p1> FnMut(&'f mut Self::P0<'p0>, Self::P1<'p1>) -> Ret,
    {
        let mut queue = self
            .starts
            .into_iter()
            .map(|e| *e.borrow())
            .collect::<VecDeque<Entity>>();

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

            queue.extend(T::entities(&relations.edges));
        }
    }
}

impl<Q, RS, F, T, E, I> ForEachPermutations<0>
    for Operations<&'_ mut Query<'_, '_, (Q, Relations<RS>), F>, (), (), T, I>
where
    Q: WorldQuery,
    RS: RelationSet,

    RS::Edges: PadMax,
    <RS::Edges as PadMax>::Padded: ReadOnlyWorldQuery,
    F: ReadOnlyWorldQuery,
    T: EdgeSide,
    E: Borrow<Entity>,
    I: IntoIterator<Item = E>,
{
    type P0<'p0> = <Q as WorldQuery>::Item<'p0>;
    type P1<'p1> = <Q as WorldQuery>::Item<'p1>;

    fn for_each<Func, Ret>(self, mut func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'p0, 'p1> FnMut(&'f mut Self::P0<'p0>, Self::P1<'p1>) -> Ret,
    {
        let mut queue = self
            .starts
            .into_iter()
            .map(|e| *e.borrow())
            .collect::<VecDeque<Entity>>();

        'queue: while let Some(entity) = queue.pop_front() {
            // SAFETY: Self referential relations are impossible so this is always safe.
            let Ok((mut control, relations)) = (unsafe { self.control.get_unchecked(entity) })
            else {
                continue;
            };

            for e in T::entities(&relations.edges) {
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

            queue.extend(T::entities(&relations.edges));
        }
    }
}

#[cfg_attr(doc, aquamarine::aquamarine)]
/// Control flow enum for [`ForEachPermutations`] and [`ForEachPermutations3Arity`]. The closures
/// accepted by both return `impl Into<ControlFlow>` with `()` being turned into
/// `ControlFlow::Continue` to save you from typing `return ControlFlow::Continue` in all your
/// functions.
///
/// ```
///# use bevy::prelude::*;
///# use aery::prelude::*;
///#
///# #[derive(Component)]
///# struct A {
///#     // ..
///# }
///#
///# #[derive(Component)]
///# struct B {
///#     // ..
///# }
///#
///# #[derive(Relation)]
///# struct R;
///#
///# fn predicate(a: &A, b: &B) -> bool {
///#     true
///# }
///#
/// fn sys(a: Query<(&A, Relations<R>)>, b: Query<&B>) {
///     a.ops().join::<R>(&b).for_each(|a, b| {
///         if predicate(a, b) {
///             return ControlFlow::Exit;
///         }
///
///         // Return types still need to be the same so explicitly providing this is nessecary
///         // when doing any controlflow manipulation.
///         ControlFlow::Continue
///     })
/// }
/// ```
///
///
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
/// fn setup(mut commands: Commands) {
///     let bs = std::array::from_fn::<_, 3, _>(|n| commands.spawn(B(n)).id());
///     let cs = std::array::from_fn::<_, 3, _>(|n| commands.spawn(C(n)).id());
///     let ds = std::array::from_fn::<_, 3, _>(|n| commands.spawn(D(n)).id());
///
///     let a = commands.spawn(A).id();
///
///     for id in bs {
///         commands.add(Set::<R0>::new(a, id));
///     }
///
///     for id in cs {
///         commands.add(Set::<R1>::new(a, id));
///     }
///
///     for id in ds {
///         commands.add(Set::<R2>::new(a, id));
///     }
/// }
///
/// fn ff_sys(
///     a: Query<(&A, Relations<(R0, R1, R2)>)>,
///     b: Query<&B>,
///     c: Query<&C>,
///     d: Query<&D>
/// ) {
///     a.ops()
///         .join::<R0>(&b)
///         .join::<R1>(&c)
///         .join::<R2>(&d)
///         .for_each(|a, (b, c, d)| {
///             if c.0 == 1 {
///                 ControlFlow::FastForward(1)
///             }
///             else {
///                 println!("({}, {}, {})", b.0, c.0, d.0);
///                 ControlFlow::Continue
///             }
///         });
/// }
/// ```
/// ### Output of ff_sys:
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
/// ## Walk Illustration:
/// ```
/// use bevy::prelude::*;
/// use aery::prelude::*;
///
/// #[derive(Component)]
/// struct A(usize);
///
/// #[derive(Component)]
/// struct B(usize);
///
/// #[derive(Relation)]
/// struct R0;
///
/// #[derive(Relation)]
/// struct R1;
///
/// fn setup(mut commands: Commands) {
///     commands.add(|wrld: &mut World| {
///         wrld.spawn(A(0))
///             .scope::<R0>(|_, mut ent1| {
///                 ent1.insert(A(1));
///                 ent1.scope_target::<R1>(|_, mut ent| { ent.insert(B(0)); })
///                     .scope_target::<R1>(|_, mut ent| { ent.insert(B(1)); });
///             })
///             .scope::<R0>(|_, mut ent2| {
///                 ent2.insert(A(2));
///                 ent2.scope_target::<R1>(|_, mut ent| { ent.insert(B(3)); })
///                     .scope_target::<R1>(|_, mut ent| { ent.insert(B(4)); });
///             })
///             .scope::<R0>(|_, mut ent3| {
///                 ent3.insert(A(3));
///                 ent3.scope_target::<R1>(|_, mut ent| { ent.insert(B(5)); })
///                     .scope_target::<R1>(|_, mut ent| { ent.insert(B(6)); });
///             });
///     });
/// }
///
/// fn walk_sys(
///     roots: Query<Entity, Root<R0>>,
///     a: Query<(&A, Relations<(R0, R1)>)>,
///     b: Query<&B>,
/// ) {
///     a.ops()
///         .join::<R1>(&b)
///         .traverse::<R0>(roots.iter())
///         .for_each(|a, a_child, b| {
///             if a_child.0 == 2 {
///                 ControlFlow::Walk
///             }
///             else {
///                 println!("({}, {}, {})", a.0, a_child.0, b.0);
///                 ControlFlow::Continue
///             }
///         });
/// }
/// ```
/// ### Output of walk_sys:
/// ```ignore
///     (0, 1, 0)
///     (0, 1, 1)
/// //  Skipped:
/// //  (0, 2, 3)
/// //  (0, 2, 4)
///     (0, 3, 5)
///     (0, 3, 6)
/// ```
/// ## Probe Illustration:
/// ```
/// use bevy::prelude::*;
/// use aery::prelude::*;
///
/// #[derive(Component)]
/// struct A(usize);
///
/// #[derive(Relation)]
/// struct R;
///
/// fn setup(mut commands: Commands) {
///     commands.add(|wrld: &mut World| {
///         wrld.spawn(A(0))
///             .scope::<R>(|_, mut ent1| {
///                 ent1.insert(A(1));
///                 ent1.scope_target::<R>(|_, mut ent4| { ent4.insert(A(4)); })
///                     .scope_target::<R>(|_, mut ent5| { ent5.insert(A(5)); });
///             })
///             .scope::<R>(|_, mut ent2| {
///                 ent2.insert(A(2));
///                 ent2.scope_target::<R>(|_, mut ent6| { ent6.insert(A(6)); })
///                     .scope_target::<R>(|_, mut ent7| { ent7.insert(A(7)); });
///             })
///             .scope::<R>(|_, mut ent3| {
///                 ent3.insert(A(3));
///                 ent3.scope_target::<R>(|_, mut ent8| { ent8.insert(A(8)); })
///                     .scope_target::<R>(|_, mut ent9| { ent9.insert(A(9)); });
///             });
///     });
/// }
///
/// fn noprobe(query: Query<(&A, Relations<R>)>, roots: Query<Entity, Root<R>>) {
///     query.ops().traverse::<R>(roots.iter()).for_each(|a, a_child| {
///         // ..
///     })
/// }
///
/// fn probe(query: Query<(&A, Relations<R>)>, roots: Query<Entity, Root<R>>) {
///     query.ops().traverse::<R>(roots.iter()).for_each(|a, a_child| {
///         if (a_child.0 == 2) {
///             ControlFlow::Probe
///         }
///         else {
///             ControlFlow::Continue
///         }
///     })
/// }
/// ```
/// ### Traversal of noprobe:
/// Pink means traversed.
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
/// E6:::pink --R--> E2:::pink
/// E7:::pink --R--> E2:::pink
///
/// E8:::pink --R--> E3:::pink
/// E9:::pink --R--> E3:::pink
/// ```
///
/// ### Traversal of probe:
/// Pink means traversed.
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
pub enum ControlFlow {
    /// Continue to next permutation.
    Continue,
    /// Stop iterating permutatiosn and exit loop.
    Exit,
    /// FastForward(n) will advance the nth join to the next match skipping any premutations.
    /// inbetween where it currently is and the next permutation where it was supposed to advance.
    /// Has no effect for operations with no joins.
    FastForward(usize),
    /// Walks to the next entity in the "traversal" skipping any remaining permutations to iterate.
    /// - For operations with *traversals* this is the next entity in the traversal path.
    /// - Otherwise when there are only joins it's a linear traversal through the query items
    /// and this is just the next entity in the control query.
    Walk,
    /// Skips:
    /// - Any remaining join permutations.
    /// - Any remaining entities on the current breadth.
    /// - Entities on the breadth of the next depth that are before the current child/ancestor.
    Probe,
    /// Conclude's a traversal path. Useful for traversals that start from multiple points like
    /// hierarchy ascent to find siblings.
    Conclude,
}

impl From<()> for ControlFlow {
    fn from(_: ()) -> Self {
        ControlFlow::Continue
    }
}
