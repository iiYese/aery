use crate::{
    relation::{EdgeWQ, Relation},
    tuple_traits::*,
};
use bevy::ecs::{
    entity::Entity,
    query::{ReadOnlyWorldQuery, WorldQuery},
    system::Query,
};
use std::{borrow::Borrow, collections::VecDeque, marker::PhantomData};

type EdgeIter<'a> = std::iter::Flatten<
    std::option::IntoIter<std::iter::Copied<indexmap::set::Iter<'a, bevy::prelude::Entity>>>,
>;

/// Struct to track inner product iteration.
pub struct EdgeProduct<'a, const N: usize> {
    pub(crate) base_iterators: [EdgeIter<'a>; N],
    pub(crate) live_iterators: [EdgeIter<'a>; N],
    pub(crate) entities: [Option<Entity>; N],
}

impl<'a, const N: usize> EdgeProduct<'a, N> {
    fn advance(&mut self, prev_matches: [bool; N]) -> Option<[Entity; N]> {
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

/// `WorldQuery` type to query for Relation types. Takes a [`RelationSet`] which is a single
/// relation or tuple of relation types. *Must appear in the second position of the outer most tuple
/// to use relation operations and no type may appear more than once for operations to work.*
/// See [`AeryQueryExt`] for operations.
#[derive(WorldQuery)]
pub struct Relations<R: RelationSet> {
    pub(crate) edges: EdgeWQ,
    _filters: R::Filters,
    _phantom: PhantomData<R>,
}

/// Struct that is used to track metadata for relation operations.
pub struct Operations<Control, JoinedTypes = (), JoinedQueries = (), Traversal = ()> {
    control: Control,
    joined_types: PhantomData<JoinedTypes>,
    joined_queries: JoinedQueries,
    traversal: Traversal,
}

/// Struct that is used to track metadata for breadth first traversals.
pub struct BreadthFirstTraversal<T, E, I>
where
    T: Relation,
    E: Borrow<Entity>,
    I: IntoIterator<Item = E>,
{
    roots: I,
    _phantom: PhantomData<(T, E)>,
}

/// An extension trait to turn `Query<(X, Relations<R>)>`s into [`Operations`]s which have the
/// trait implementations to build relation operations. This query is called the "control query".
/// The [`RelationSet`] `R` from this query is what is used for joins and traversals any `T` in a
/// subsequent `.join::<T>(_)` or `.beadth_first::<T>(_)` call must be present in `R`.
/// Also see [`Join`] and [`BreadthFirst`].
pub trait AeryQueryExt {
    /// Provides read only access to the left portion of the [`Query`] tuple.
    fn ops(&self) -> Operations<&Self>;
    /// Provides mutable access to the left portion of the [`Query`] tuple.
    fn ops_mut(&mut self) -> Operations<&mut Self>;
}

impl<'w, 's, Q, F, R> AeryQueryExt for Query<'w, 's, (Q, Relations<R>), F>
where
    Q: WorldQuery,
    F: ReadOnlyWorldQuery,
    R: RelationSet + Send + Sync,
{
    fn ops(&self) -> Operations<&Self> {
        Operations {
            control: self,
            joined_types: PhantomData,
            joined_queries: (),
            traversal: (),
        }
    }

    fn ops_mut(&mut self) -> Operations<&mut Self> {
        Operations {
            control: self,
            joined_types: PhantomData,
            joined_queries: (),
            traversal: (),
        }
    }
}

/// A trait to implement the `breadth_first` functionality of the operations API. Any `T` in
/// `breadth_first::<T>(roots)` must be present in the [`RelationSet`] of the control query.
/// Diamonds are impossible with `Exclusive` relations where the edges face bottom up instead of
/// top down. For this reason bottom up graphs are the only pattern that is recognized for
/// traversal.
/// Uses:
/// - [`ForEachPermutations`] for operations with just traversals.
/// - [`ForEachPermutations3Arity`] for operations with traversals and joins.
///
/// See [`Join`] for joining queries.
/// # Illustration:
/// ```
/// use bevy::prelude::*;
/// use aery::prelude::*;
///
/// #[derive(Component)]
/// struct A;
///
/// #[derive(Relation)]
/// struct R;
///
/// fn setup(mut commands: Commands) {
///     let [e0, e1, e2, e3, e4, e5, e6] = std::array::from_fn(|_| commands.spawn_empty().id());
///
///     for (from, to) in [
///         (e1, e0),
///         (e2, e0),
///         (e3, e1),
///         (e4, e1),
///         (e5, e2),
///         (e6, e2)
///     ] {
///         commands.set::<R>(from, to);
///     }
///
///     //  Will construct the following graph:
///     //
///     //        0
///     //       / \
///     //      /   \
///     //     /     \
///     //    1       2
///     //   / \     / \
///     //  3   4   5   6
/// }
///
/// fn sys(a: Query<(&A, Relations<R>)>, roots: Query<Entity, Root<R>>) {
///     a.ops().breadth_first::<R>(roots.iter()).for_each(|a_ancestor, a| {
///         // Will traverse in the following order:
///         // (a_ancestor, a): (0, 1)
///         // (a_ancestor, a): (0, 2)
///         // (a_ancestor, a): (1, 3)
///         // (a_ancestor, a): (1, 4)
///         // (a_ancestor, a): (2, 5)
///         // (a_ancestor, a): (2, 6)
///     })
/// }
/// ```
/// What the Archetypes/Tables should look like:
///
/// | entityid  | A | `Root<R>` |
/// |-----------|---|-----------|
/// | 0         | _ | _         |
///
/// *Note:* `Root<_>` markers are automatically added and removed by the provided commands for
/// convenient traversing.
///
/// | entityid  | A | R |
/// |-----------|---|---|
/// | 1         | _ | 0 |
/// | 2         | _ | 0 |
/// | 3         | _ | 1 |
/// | 4         | _ | 1 |
/// | 5         | _ | 2 |
/// | 6         | _ | 2 |
pub trait BreadthFirst<E, I>
where
    E: Borrow<Entity>,
    I: IntoIterator<Item = E>,
{
    type Out<T: Relation>;
    fn breadth_first<T: Relation>(self, roots: I) -> Self::Out<T>;
}

impl<Control, JoinedTypes, JoinedQueries, Traversal, E, I> BreadthFirst<E, I>
    for Operations<Control, JoinedTypes, JoinedQueries, Traversal>
where
    E: Borrow<Entity>,
    I: IntoIterator<Item = E>,
{
    type Out<T: Relation> =
        Operations<Control, JoinedTypes, JoinedQueries, BreadthFirstTraversal<T, E, I>>;

    fn breadth_first<T: Relation>(self, roots: I) -> Self::Out<T> {
        Operations {
            control: self.control,
            joined_types: self.joined_types,
            joined_queries: self.joined_queries,
            traversal: BreadthFirstTraversal {
                roots,
                _phantom: PhantomData,
            },
        }
    }
}

/// A trait to implement the `join` functionality of the operations API. Any `T` in
/// `join::<T>(query)` must be present in the [`RelationSet`] of the control query. The type of
/// join performed is what's known as an "inner join" which produces permutations of all matched
/// entiteis.
/// - [`ForEachPermutations`] for operations with just joins.
/// - [`ForEachPermutations3Arity`] for operations with joins and traversals.
///
/// See [`BreadthFirst`] for traversing hierarchies.
/// # Illustration:
/// ```
/// use bevy::prelude::*;
/// use aery::prelude::*;
///
/// #[derive(Component)]
/// struct A(&'static str);
///
/// #[derive(Component)]
/// struct B(&'static str);
///
/// #[derive(Component)]
/// struct C(&'static str);
///
/// #[derive(Relation)]
/// struct R0;
///
///
/// #[derive(Relation)]
/// #[multi]
/// struct R1;
///
/// fn sys(a: Query<(&A, Relations<(R0, R1)>)>, b: Query<&B>, c: Query<&C>) {
///     a.ops()
///         .join::<R0>(&b)
///         .join::<R1>(&c)
///         .for_each(|a, (b, c)| {
///             // stuff
///         })
///
///     //  If we used the "Entity in Component" pattern the equivalent without Aery would be:
///     //  for (a, r0s, r1s) in &a {
///     //      for r0 in r0s {
///     //          let Ok(b) = b.get(*r0) else {
///     //              continue
///     //          };
///     //
///     //          for r1 in r1s {
///     //              let Ok(c) = c.get(*r1) else {
///     //                  continue
///     //              };
///     //
///     //              // stuff
///     //
///     //          }
///     //      }
///     //  }
/// }
/// ```
/// **If `a: Query<(&A, Relations<(R0, R1)>)>`:**
///
/// | entityid  | A     | R0    | R1        |
/// |-----------|-------|-------|-----------|
/// | 0         | `"X"` | 3     | 6, 10, 7  |
/// | 1         | `"Y"` | 4     | 5, 6      |
/// | 2         | `"Z"` | 5     | 9         |
///
/// **and `b: Query<&B>`:**
///
/// | entityid  | B             |
/// |-----------|---------------|
/// | 3         | `"foo"`       |
/// | 5         | `"bar"`       |
///
/// **and `c: Query<&C>`:**
///
/// | entityid  | C             |
/// |-----------|---------------|
/// | 6         | `"baz"`       |
/// | 7         | `"qux"`       |
/// | 8         | `"corge"`     |
/// | 9         | `"grault"`    |
///
/// **then `a.ops().join::<R0>(&b).join::<R1>(&c).for_each(|a, (b, c)| {})`:**
///
/// | a     | b         | c         |
/// |-------|-----------|-----------|
/// | `"X"` | `"foo"`   | "baz"     |
/// | `"X"` | `"foo"`   | "qux"     |
/// | `"Z"` | `"bar"`   | "grault"  |
pub trait Join<Item>
where
    Item: for<'a> Joinable<'a, 1>,
{
    type Out<T: Relation>;
    fn join<T: Relation>(self, item: Item) -> Self::Out<T>;
}

impl<Item, Control, JoinedTypes, JoinedQueries, Traversal> Join<Item>
    for Operations<Control, JoinedTypes, JoinedQueries, Traversal>
where
    Item: for<'a> Joinable<'a, 1>,
    JoinedTypes: Append,
    JoinedQueries: Append,
{
    type Out<T: Relation> = Operations<
        Control,
        <JoinedTypes as Append>::Out<T>,
        <JoinedQueries as Append>::Out<Item>,
        Traversal,
    >;

    fn join<T: Relation>(self, item: Item) -> Self::Out<T> {
        Operations {
            control: self.control,
            joined_types: PhantomData,
            joined_queries: Append::append(self.joined_queries, item),
            traversal: self.traversal,
        }
    }
}

/// Control flow enum for [`ForEachPermutations`] and [`ForEachPermutations3Arity`].
/// ```
/// use bevy::prelude::*;
/// use aery::prelude::*;
/// #[derive(Component)]
/// struct A {
///     // ..
/// }
///
/// #[derive(Component)]
/// struct B {
///     // ..
/// }
///
/// #[derive(Relation)]
/// struct R;
///
/// fn predicate(a: &A, b: &B) -> bool {
///     # true // amongus
///     // ..
/// }
///
/// fn sys(a: Query<(&A, Relations<R>)>, b: Query<&B>) {
///     a.ops().join::<R>(&b).for_each(|a, b| {
///         if predicate(a, b) {
///             return ControlFlow::Exit;
///         }
///
///         // `()` impls Into<ControlFlow> for convenience. Return types still need to be the same
///         // so explicitly providing this is nessecary when doing any controlflow manipulation.
///         ControlFlow::Continue
///     })
/// }
/// ```
pub enum ControlFlow {
    /// Continue to next permutation.
    Continue,
    /// Stop iterating permutatiosn and exit loop.
    Exit,
    /// FastForward(n) will advance the nth join to the next match skipping any premutations
    /// inbetween where it currently is and the next permutation where it was supposed to advance.
    FastForward(usize),
    /// Walks to the next entity in the traversal skipping any remaining permutations to iterate.
    /// - For beadth first traversals this is the next entity in the walk path.
    /// - Otherwise it's a linear traversal through the query items and this is the next entity.
    Walk,
    /// Immediately probe a traversal. Skips any remaining permutations and the remaining walk
    /// path.
    Probe,
}

impl From<()> for ControlFlow {
    fn from(_: ()) -> Self {
        ControlFlow::Continue
    }
}

/// A for each trait to get around lending Iterators not being expressible with current GATs.
/// When iterating:
/// - Diamonds
/// - Cycles
/// - Joins where multiple entities have the same target
///
/// References to the same entities components can be produced more than once which is why this
/// problem cannot be solved with `unsafe`. So to work around this "lifetime trapping" is used
/// instead. The closure parameters cannot escape the closure.
///
/// For any control query `Query<(X, Relations<..>)>`:
/// - If there is only joins: Permutations of valid entities from the joined queries will be looped
/// through. The left parameter will be the `X` item and the right parameter will be a tuple of all
/// the query fetch parameters from the joined queries.
/// - If there is only hierarchy traversals: Traversable ancestor-descendant permutations that
/// belong to the control query will be looped through. The left parameter will be the `X` item of
/// an ancestor and the right parameter will be the `X` item of an immediate descendant.
///
/// See [`ControlFlow`] for control flow options and [`ForEachPermutations3Arity`] for the loop
/// behavior of operations with hierarchy traversals and 1 or more join.
pub trait ForEachPermutations<const N: usize> {
    type Left<'l>;
    type Right<'r>;

    fn for_each<Func, Ret>(self, func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'l, 'r> FnMut(&'f mut Self::Left<'l>, Self::Right<'r>) -> Ret;
}

impl<Q, R, F, JoinedTypes, JoinedQueries, const N: usize> ForEachPermutations<N>
    for Operations<&'_ Query<'_, '_, (Q, Relations<R>), F>, JoinedTypes, JoinedQueries, ()>
where
    Q: WorldQuery,
    R: RelationSet,
    F: ReadOnlyWorldQuery,
    JoinedTypes: Product<N>,
    JoinedQueries: for<'a> Joinable<'a, N>,
{
    type Left<'l> = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'l>;
    type Right<'r> = <JoinedQueries as Joinable<'r, N>>::Out;

    fn for_each<Func, Ret>(mut self, mut func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'l, 'r> FnMut(&'f mut Self::Left<'l>, Self::Right<'r>) -> Ret,
    {
        for (mut control, relations) in self.control.iter() {
            let mut edge_product = JoinedTypes::product(relations.edges);
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
                    ControlFlow::Exit => return,
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
    JoinedTypes: Product<N>,
    JoinedQueries: for<'a> Joinable<'a, N>,
{
    type Left<'l> = <Q as WorldQuery>::Item<'l>;
    type Right<'r> = <JoinedQueries as Joinable<'r, N>>::Out;

    fn for_each<Func, Ret>(mut self, mut func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'l, 'r> FnMut(&'f mut Self::Left<'l>, Self::Right<'r>) -> Ret,
    {
        for (mut control, relations) in self.control.iter_mut() {
            let mut edge_product = JoinedTypes::product(relations.edges);
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
                    ControlFlow::Exit => return,
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

impl<Q, R, F, T, E, I> ForEachPermutations<0>
    for Operations<&'_ Query<'_, '_, (Q, Relations<R>), F>, (), (), BreadthFirstTraversal<T, E, I>>
where
    Q: WorldQuery,
    R: RelationSet,
    F: ReadOnlyWorldQuery,
    T: Relation,
    E: Borrow<Entity>,
    I: IntoIterator<Item = E>,
{
    type Left<'l> = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'l>;
    type Right<'r> = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'r>;

    fn for_each<Func, Ret>(self, mut func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'l, 'r> FnMut(&'f mut Self::Left<'l>, Self::Right<'r>) -> Ret,
    {
        let mut queue = self
            .traversal
            .roots
            .into_iter()
            .map(|e| *e.borrow())
            .collect::<VecDeque<Entity>>();

        'queue: while let Some(entity) = queue.pop_front() {
            let Ok((mut control, relations)) = self.control.get(entity) else {
                continue
            };

            for e in relations.edges.edges.iter_hosts::<T>() {
                let Ok(joined_queries) = self.control.get(e) else {
                    continue
                };

                match func(&mut control, joined_queries.0).into() {
                    ControlFlow::Exit => return,
                    ControlFlow::Probe => {
                        queue.clear();
                        queue.push_back(e);
                        continue 'queue;
                    }
                    _ => {}
                }
            }

            for e in relations.edges.edges.iter_hosts::<T>() {
                queue.push_back(e);
            }
        }
    }
}

impl<Q, R, F, T, E, I> ForEachPermutations<0>
    for Operations<
        &'_ mut Query<'_, '_, (Q, Relations<R>), F>,
        (),
        (),
        BreadthFirstTraversal<T, E, I>,
    >
where
    Q: WorldQuery,
    R: RelationSet,
    F: ReadOnlyWorldQuery,
    T: Relation,
    E: Borrow<Entity>,
    I: IntoIterator<Item = E>,
{
    type Left<'l> = <Q as WorldQuery>::Item<'l>;
    type Right<'r> = <Q as WorldQuery>::Item<'r>;

    fn for_each<Func, Ret>(self, mut func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'l, 'r> FnMut(&'f mut Self::Left<'l>, Self::Right<'r>) -> Ret,
    {
        let mut queue = self
            .traversal
            .roots
            .into_iter()
            .map(|e| *e.borrow())
            .collect::<VecDeque<Entity>>();

        'queue: while let Some(entity) = queue.pop_front() {
            // SAFETY: Self referential relations are impossible so this is always safe.
            let Ok((mut control, relations)) = (unsafe {
                self.control.get_unchecked(entity)
            }) else {
                continue
            };

            for e in relations.edges.edges.iter_hosts::<T>() {
                // SAFETY: Self referential relations are impossible so this is always safe.
                let Ok(joined_queries) = (unsafe { self.control.get_unchecked(e) }) else {
                    continue
                };

                match func(&mut control, joined_queries.0).into() {
                    ControlFlow::Exit => return,
                    ControlFlow::Probe => {
                        queue.clear();
                        queue.push_back(e);
                        continue 'queue;
                    }
                    _ => {}
                }
            }

            for e in relations.edges.edges.iter_hosts::<T>() {
                queue.push_back(e);
            }
        }
    }
}

/// A 3 arity version of [`ForEachPermutations`] for when operations feature a traversal with 1 or
/// more joins. Will iterate through hierarchy permutations and join permutations together.
/// - The left paramater will be an ancestor entity.
/// - The middle parameter will be a descendant of the ancestor.
/// - The right parameter will be a tuple of all the query fetch parameters from joined queries
/// where the entity being joined on is the descendant. The traversal relation is essentially
/// treated as another join paramter where the query being joined on is the control query.
pub trait ForEachPermutations3Arity<const N: usize> {
    type Left<'l>;
    type Middle<'m>;
    type Right<'r>;

    fn for_each<Func, Ret>(self, func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'l, 'm, 'r> FnMut(
            &'f mut Self::Left<'l>,
            &'f mut Self::Middle<'m>,
            Self::Right<'r>,
        ) -> Ret;
}

impl<Q, R, F, T, E, I, JoinedTypes, JoinedQueries, const N: usize> ForEachPermutations3Arity<N>
    for Operations<
        &'_ Query<'_, '_, (Q, Relations<R>), F>,
        JoinedTypes,
        JoinedQueries,
        BreadthFirstTraversal<T, E, I>,
    >
where
    Q: WorldQuery,
    R: RelationSet,
    F: ReadOnlyWorldQuery,
    T: Relation,
    E: Borrow<Entity>,
    I: IntoIterator<Item = E>,
    JoinedTypes: Product<N>,
    JoinedQueries: for<'a> Joinable<'a, N>,
{
    type Left<'l> = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'l>;
    type Middle<'m> = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'m>;
    type Right<'r> = <JoinedQueries as Joinable<'r, N>>::Out;

    fn for_each<Func, Ret>(mut self, mut func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'l, 'm, 'r> FnMut(
            &'f mut Self::Left<'l>,
            &'f mut Self::Middle<'m>,
            Self::Right<'r>,
        ) -> Ret,
    {
        let mut queue = self
            .traversal
            .roots
            .into_iter()
            .map(|e| *e.borrow())
            .collect::<VecDeque<Entity>>();

        'queue: while let Some(entity) = queue.pop_front() {
            let Ok((mut ancestor_components, ancestor_edges)) = self.control.get(entity) else {
                continue
            };

            for descendant in ancestor_edges.edges.edges.iter_hosts::<T>() {
                let Ok((mut descendant_components, descendant_edges)) = self
                    .control
                    .get(descendant)
                else {
                    continue
                };

                let mut edge_product = JoinedTypes::product(descendant_edges.edges);
                let mut matches = [false; N];

                while let Some(entities) = edge_product.advance(matches) {
                    matches = Joinable::check(&self.joined_queries, entities);

                    if matches.iter().any(|b| !b) {
                        continue;
                    }

                    match func(
                        &mut ancestor_components,
                        &mut descendant_components,
                        Joinable::join(&mut self.joined_queries, entities),
                    )
                    .into()
                    {
                        ControlFlow::Continue => {}
                        ControlFlow::Exit => return,
                        ControlFlow::Walk => break,
                        ControlFlow::FastForward(n) if n < N => {
                            matches[n] = false;
                        }
                        ControlFlow::Probe => {
                            queue.clear();
                            queue.push_back(descendant);
                            continue 'queue;
                        }
                        _ => {}
                    }
                }
            }

            for e in ancestor_edges.edges.edges.iter_hosts::<T>() {
                queue.push_back(e);
            }
        }
    }
}

impl<Q, R, F, T, E, I, JoinedTypes, JoinedQueries, const N: usize> ForEachPermutations3Arity<N>
    for Operations<
        &'_ mut Query<'_, '_, (Q, Relations<R>), F>,
        JoinedTypes,
        JoinedQueries,
        BreadthFirstTraversal<T, E, I>,
    >
where
    Q: WorldQuery,
    R: RelationSet,
    F: ReadOnlyWorldQuery,
    T: Relation,
    E: Borrow<Entity>,
    I: IntoIterator<Item = E>,
    JoinedTypes: Product<N>,
    JoinedQueries: for<'a> Joinable<'a, N>,
{
    type Left<'l> = <Q as WorldQuery>::Item<'l>;
    type Middle<'m> = <Q as WorldQuery>::Item<'m>;
    type Right<'r> = <JoinedQueries as Joinable<'r, N>>::Out;

    fn for_each<Func, Ret>(mut self, mut func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'l, 'm, 'r> FnMut(
            &'f mut Self::Left<'l>,
            &'f mut Self::Middle<'m>,
            Self::Right<'r>,
        ) -> Ret,
    {
        let mut queue = self
            .traversal
            .roots
            .into_iter()
            .map(|e| *e.borrow())
            .collect::<VecDeque<Entity>>();

        'queue: while let Some(entity) = queue.pop_front() {
            // SAFETY: Self referential relations are impossible so this is always safe.
            let Ok((mut ancestor_components, ancestor_edges)) = (unsafe {
                self.control.get_unchecked(entity)
            }) else {
                continue
            };

            for descendant in ancestor_edges.edges.edges.iter_hosts::<T>() {
                // SAFETY: Self referential relations are impossible so this is always safe.
                let Ok((mut descendant_components, descendant_edges)) = (unsafe {
                    self.control.get_unchecked(descendant)
                }) else {
                    continue
                };

                let mut edge_product = JoinedTypes::product(descendant_edges.edges);
                let mut matches = [false; N];

                while let Some(entities) = edge_product.advance(matches) {
                    matches = Joinable::check(&self.joined_queries, entities);

                    if matches.iter().any(|b| !b) {
                        continue;
                    }

                    match func(
                        &mut ancestor_components,
                        &mut descendant_components,
                        Joinable::join(&mut self.joined_queries, entities),
                    )
                    .into()
                    {
                        ControlFlow::Continue => {}
                        ControlFlow::Exit => return,
                        ControlFlow::Walk => break,
                        ControlFlow::FastForward(n) if n < N => {
                            matches[n] = false;
                        }
                        ControlFlow::Probe => {
                            queue.clear();
                            queue.push_back(descendant);
                            continue 'queue;
                        }
                        _ => {}
                    }
                }
            }
            for e in ancestor_edges.edges.edges.iter_hosts::<T>() {
                queue.push_back(e);
            }
        }
    }
}

#[cfg(test)]
#[allow(dead_code)]
#[allow(unused_variables)]
mod compile_tests {
    use crate::prelude::*;
    use bevy::prelude::*;

    #[derive(Component)]
    struct A;

    #[derive(Component)]
    struct B;

    #[derive(Component)]
    struct C;

    #[derive(Relation)]
    #[cleanup(policy = "Counted")]
    #[multi]
    struct R0;

    #[derive(Relation)]
    struct R1;

    fn join_immut(left: Query<(&A, Relations<(R0, R1)>)>, b: Query<&B>, c: Query<&C>) {
        left.ops()
            .join::<R0>(&b)
            .join::<R1>(&c)
            .for_each(|a, (b, c)| {});
    }

    fn join_left_mut(mut left: Query<(&mut A, Relations<(R0, R1)>)>, b: Query<&C>, c: Query<&C>) {
        left.ops_mut()
            .join::<R0>(&b)
            .join::<R1>(&c)
            .for_each(|a, (b, c)| {});
    }

    fn join_right_mut(
        left: Query<(&A, Relations<(R0, R1)>)>,
        mut b: Query<&mut B>,
        mut c: Query<&mut C>,
    ) {
        left.ops()
            .join::<R0>(&mut b)
            .join::<R1>(&mut c)
            .for_each(|a, (b, c)| {});
    }

    fn join_full_mut(
        mut left: Query<(&mut A, Relations<(R0, R1)>)>,
        mut b: Query<&mut B>,
        mut c: Query<&mut C>,
    ) {
        left.ops_mut()
            .join::<R0>(&mut b)
            .join::<R1>(&mut c)
            .for_each(|a, (b, c)| {});
    }

    fn breadth_first_immut(left: Query<(&A, Relations<(R0, R1)>)>) {
        left.ops()
            .breadth_first::<R0>(None::<Entity>)
            .for_each(|a0, a1| {});
    }

    fn breadth_first_immut_joined(left: Query<(&A, Relations<(R0, R1)>)>, right: Query<&B>) {
        left.ops()
            .breadth_first::<R0>(None::<Entity>)
            .join::<R1>(&right)
            .for_each(|a0, a1, b| {});
    }

    fn breadth_first_mut(mut left: Query<(&mut A, Relations<(R0, R1)>)>) {
        left.ops_mut()
            .breadth_first::<R0>(None::<Entity>)
            .for_each(|a0, a1| {});
    }

    fn breadth_first_mut_joined_mut(
        left: Query<(&A, Relations<(R0, R1)>)>,
        mut right: Query<&mut B>,
    ) {
        left.ops()
            .breadth_first::<R0>(None::<Entity>)
            .join::<R1>(&mut right)
            .for_each(|a0, a1, b| {});
    }
}

#[cfg(test)]
mod tests {
    use crate::prelude::*;
    use bevy::{app::AppExit, prelude::*};

    #[derive(Component)]
    struct S;

    #[derive(Component, Debug)]
    struct A(i32);

    #[derive(Component, Debug)]
    struct B(i32);

    #[derive(Component, Debug)]
    struct C(i32);

    #[derive(Relation)]
    #[multi]
    struct R0;

    #[derive(Relation)]
    #[multi]
    struct R1;

    #[derive(Relation)]
    #[multi]
    struct R2;

    #[derive(Resource)]
    struct EntityList {
        entities: [Entity; 9],
    }

    #[test]
    fn left_scarce_permutations_o() {
        fn init(world: &mut World) {
            let entities = [
                world.spawn(A(0)).id(),
                world.spawn(A(0)).id(),
                world.spawn(A(0)).id(),
                //
                world.spawn(B(0)).id(),
                world.spawn(B(0)).id(),
                world.spawn(B(0)).id(),
                //
                world.spawn(C(0)).id(),
                world.spawn(C(0)).id(),
                world.spawn(C(0)).id(),
            ];

            let [_, a1, _, b0, _, b2, _, c1, _] = entities;

            let left = world.spawn(S).id();

            world.set::<R0>(left, a1);
            world.set::<R1>(left, b0);
            world.set::<R1>(left, b2);
            world.set::<R2>(left, c1);

            world.insert_resource(EntityList { entities });
        }

        fn run(
            left: Query<(&S, Relations<(R0, R1, R2)>)>,
            mut a: Query<&mut A>,
            mut b: Query<&mut B>,
            mut c: Query<&mut C>,
        ) {
            left.ops()
                .join::<R0>(&mut a)
                .join::<R1>(&mut b)
                .join::<R2>(&mut c)
                .for_each(|_, (mut a, mut b, mut c)| {
                    a.0 += 1;
                    b.0 += 1;
                    c.0 += 1;
                });
        }

        fn test(
            mut exit: EventWriter<AppExit>,
            entity_list: Res<EntityList>,
            a: Query<&A>,
            b: Query<&B>,
            c: Query<&C>,
        ) {
            let [a0, a1, a2, b0, b1, b2, c0, c1, c2] = entity_list.entities;

            assert_eq!(0, a.get(a0).unwrap().0);
            assert_eq!(2, a.get(a1).unwrap().0);
            assert_eq!(0, a.get(a2).unwrap().0);

            assert_eq!(1, b.get(b0).unwrap().0);
            assert_eq!(0, b.get(b1).unwrap().0);
            assert_eq!(1, b.get(b2).unwrap().0);

            assert_eq!(0, c.get(c0).unwrap().0);
            assert_eq!(2, c.get(c1).unwrap().0);
            assert_eq!(0, c.get(c2).unwrap().0);

            exit.send(AppExit);
        }

        App::new()
            .add_plugin(Aery)
            .add_systems((init, run, test).chain())
            .run();
    }

    #[test]
    fn left_scarce_permutations_x() {
        fn init(world: &mut World) {
            let entities = [
                world.spawn(A(0)).id(),
                world.spawn(A(0)).id(),
                world.spawn(A(0)).id(),
                //
                world.spawn(B(0)).id(),
                world.spawn(B(0)).id(),
                world.spawn(B(0)).id(),
                //
                world.spawn(C(0)).id(),
                world.spawn(C(0)).id(),
                world.spawn(C(0)).id(),
            ];

            let [a0, _, a2, _, b1, _, c0, _, c2] = entities;

            let left = world.spawn(S).id();

            world.set::<R0>(left, a0);
            world.set::<R0>(left, a2);
            world.set::<R1>(left, b1);
            world.set::<R2>(left, c0);
            world.set::<R2>(left, c2);

            world.insert_resource(EntityList { entities });
        }

        fn run(
            left: Query<(&S, Relations<(R0, R1, R2)>)>,
            mut a: Query<&mut A>,
            mut b: Query<&mut B>,
            mut c: Query<&mut C>,
        ) {
            left.ops()
                .join::<R0>(&mut a)
                .join::<R1>(&mut b)
                .join::<R2>(&mut c)
                .for_each(|_, (mut a, mut b, mut c)| {
                    a.0 += 1;
                    b.0 += 1;
                    c.0 += 1;
                });
        }

        fn test(
            mut exit: EventWriter<AppExit>,
            entity_list: Res<EntityList>,
            a: Query<&A>,
            b: Query<&B>,
            c: Query<&C>,
        ) {
            let [a0, a1, a2, b0, b1, b2, c0, c1, c2] = entity_list.entities;

            assert_eq!(2, a.get(a0).unwrap().0);
            assert_eq!(0, a.get(a1).unwrap().0);
            assert_eq!(2, a.get(a2).unwrap().0);

            assert_eq!(0, b.get(b0).unwrap().0);
            assert_eq!(4, b.get(b1).unwrap().0);
            assert_eq!(0, b.get(b2).unwrap().0);

            assert_eq!(2, c.get(c0).unwrap().0);
            assert_eq!(0, c.get(c1).unwrap().0);
            assert_eq!(2, c.get(c2).unwrap().0);

            exit.send(AppExit);
        }

        App::new()
            .add_plugin(Aery)
            .add_systems((init, run, test).chain())
            .run();
    }

    #[test]
    fn left_abundant_permutations_o() {
        fn init(world: &mut World) {
            let entities = [
                world.spawn_empty().id(),
                world.spawn(A(0)).id(),
                world.spawn_empty().id(),
                //
                world.spawn(B(0)).id(),
                world.spawn_empty().id(),
                world.spawn(B(0)).id(),
                //
                world.spawn_empty().id(),
                world.spawn(C(0)).id(),
                world.spawn_empty().id(),
            ];

            let [a0, a1, a2, b0, b1, b2, c0, c1, c2] = entities;

            let left = world.spawn(S).id();

            world.set::<R0>(left, a0);
            world.set::<R0>(left, a1);
            world.set::<R0>(left, a2);
            world.set::<R1>(left, b0);
            world.set::<R1>(left, b1);
            world.set::<R1>(left, b2);
            world.set::<R2>(left, c0);
            world.set::<R2>(left, c1);
            world.set::<R2>(left, c2);

            world.insert_resource(EntityList { entities });
        }

        fn run(
            left: Query<(&S, Relations<(R0, R1, R2)>)>,
            mut a: Query<&mut A>,
            mut b: Query<&mut B>,
            mut c: Query<&mut C>,
        ) {
            left.ops()
                .join::<R0>(&mut a)
                .join::<R1>(&mut b)
                .join::<R2>(&mut c)
                .for_each(|_, (mut a, mut b, mut c)| {
                    a.0 += 1;
                    b.0 += 1;
                    c.0 += 1;
                });
        }

        fn test(
            mut exit: EventWriter<AppExit>,
            entity_list: Res<EntityList>,
            a: Query<&A>,
            b: Query<&B>,
            c: Query<&C>,
        ) {
            let [_, a1, _, b0, _, b2, _, c1, _] = entity_list.entities;

            assert_eq!(2, a.get(a1).unwrap().0);
            assert_eq!(1, b.get(b0).unwrap().0);
            assert_eq!(1, b.get(b2).unwrap().0);
            assert_eq!(2, c.get(c1).unwrap().0);

            exit.send(AppExit);
        }

        App::new()
            .add_plugin(Aery)
            .add_systems((init, run, test).chain())
            .run();
    }

    #[test]
    fn left_abundant_permutations_x() {
        fn init(world: &mut World) {
            let entities = [
                world.spawn(A(0)).id(),
                world.spawn_empty().id(),
                world.spawn(A(0)).id(),
                //
                world.spawn_empty().id(),
                world.spawn(B(0)).id(),
                world.spawn_empty().id(),
                //
                world.spawn(C(0)).id(),
                world.spawn_empty().id(),
                world.spawn(C(0)).id(),
            ];

            let [a0, a1, a2, b0, b1, b2, c0, c1, c2] = entities;

            let left = world.spawn(S).id();

            world.set::<R0>(left, a0);
            world.set::<R0>(left, a1);
            world.set::<R0>(left, a2);
            world.set::<R1>(left, b0);
            world.set::<R1>(left, b1);
            world.set::<R1>(left, b2);
            world.set::<R2>(left, c0);
            world.set::<R2>(left, c1);
            world.set::<R2>(left, c2);

            world.insert_resource(EntityList { entities });
        }

        fn run(
            left: Query<(&S, Relations<(R0, R1, R2)>)>,
            mut a: Query<&mut A>,
            mut b: Query<&mut B>,
            mut c: Query<&mut C>,
        ) {
            left.ops()
                .join::<R0>(&mut a)
                .join::<R1>(&mut b)
                .join::<R2>(&mut c)
                .for_each(|_, (mut a, mut b, mut c)| {
                    a.0 += 1;
                    b.0 += 1;
                    c.0 += 1;
                });
        }

        fn test(
            mut exit: EventWriter<AppExit>,
            entity_list: Res<EntityList>,
            a: Query<&A>,
            b: Query<&B>,
            c: Query<&C>,
        ) {
            let [a0, _, a2, _, b1, _, c0, _, c2] = entity_list.entities;

            assert_eq!(2, a.get(a0).unwrap().0);
            assert_eq!(2, a.get(a2).unwrap().0);
            assert_eq!(4, b.get(b1).unwrap().0);
            assert_eq!(2, c.get(c0).unwrap().0);
            assert_eq!(2, c.get(c2).unwrap().0);

            exit.send(AppExit);
        }

        App::new()
            .add_plugin(Aery)
            .add_systems((init, run, test).chain())
            .run();
    }
}
