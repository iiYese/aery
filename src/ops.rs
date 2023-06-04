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
    fn advance(&mut self, matches: [bool; N]) -> Option<[Entity; N]> {
        let n = matches
            .iter()
            .enumerate()
            .find_map(|(n, matches)| (!matches).then_some(n))
            .unwrap_or(N);

        for i in (n..N).skip(1) {
            self.live_iterators[i] = self.base_iterators[i].clone();
            self.entities[i] = self.live_iterators[i].next();
        }

        'l: {
            for i in (0..n).skip(1).rev() {
                if let Some(entity) = self.live_iterators[i].next() {
                    self.entities[i] = Some(entity);
                    break 'l;
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

/// [`WorldQuery`] type to query for Relation types. Takes a [`RelationSet`] which is a single
/// relation or tuple of relation types. *Must appear in the second position of the outer most tuple
/// to use relation operations.* See [`AeryQueryExt`] for how to use operations.
#[derive(WorldQuery)]
pub struct Relations<R: RelationSet> {
    pub(crate) edges: EdgeWQ,
    _filters: R::Filters,
    _phantom: PhantomData<R>,
}

/// Struct that is used to track metadata for relation operations.
pub struct Ops<Left, Joins = (), Right = (), Traversal = ()> {
    left: Left,
    joins: PhantomData<Joins>,
    right: Right,
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

mod sealed {
    use super::*;

    /// Trait to turn a [`Query`] into an [`Ops`] struct so relation operations can be performed.
    /// See [`Join`] and [`BreadthFirst`] for how to perform joins and traversals.
    pub trait AeryQueryExt {
        /// Provides read only access to the left portion of the [`Query`] tuple.
        fn ops(&self) -> Ops<&Self>;
        /// Provides mutable access to the left portion of the [`Query`] tuple.
        fn ops_mut(&mut self) -> Ops<&mut Self>;
    }

    impl<'w, 's, Q, F, R> AeryQueryExt for Query<'w, 's, (Q, Relations<R>), F>
    where
        Q: WorldQuery,
        F: ReadOnlyWorldQuery,
        R: RelationSet + Send + Sync,
    {
        fn ops(&self) -> Ops<&Self> {
            Ops {
                left: self,
                joins: PhantomData,
                right: (),
                traversal: (),
            }
        }

        fn ops_mut(&mut self) -> Ops<&mut Self> {
            Ops {
                left: self,
                joins: PhantomData,
                right: (),
                traversal: (),
            }
        }
    }
}

pub use sealed::*;

/// Trait to implement the `breadth_first` functionality of the operations API. Any `T` in
/// `breadth_first::<T>(roots)` must be present in the `Relations<(..)>` parameter of a query.
/// Diamonds are impossible with `Exclusive` relations where the edges face bottom up instead of
/// top down so this the only type of hierarchy supported. Any other patern will not be recognised
/// for traversal. See [`Join`] for performing joins.
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
///     let [e0, e1, e2, e3, e3, e5, e6] = std::array::from_fn(|_| commands.spawn().id());
///
///     [(e1, e0), (e2, e0), (e3, e1), (e4, e1), (e5, e2), (e6, e2)]
///         .into_iter()
///         .map(|(from, to)| Set { foster: from, target: to })
///         .for_each(|set| commands.add(set));
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
/// fn sys(a: Query<(&A, Relations<R>)>, roots: Query<Entity, RootOf<R>>) {
///     a.ops().breadth_first::<R>(roots.iter()).for_each(|a| {
///         // Will traverse in the following order:
///         // 0 -> 1 -> 2 -> 3 -> 4 -> 5 -> 6
///     })
/// }
/// ```
/// Resulting Archetypes/Tables:
///
/// | entityid  | A | Root<R> |
/// |-----------|---|---------|
/// | 0         | _ | _       |
///
/// *Note:* `Root<_>` markers are automatically added and removed by the [`Set`], [`UnSet`] and
/// [`CheckedDespawn`] commands. Roots of graphs are tracked for convenient traversing.
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

impl<Left, Joins, Right, Traversal, E, I> BreadthFirst<E, I> for Ops<Left, Joins, Right, Traversal>
where
    E: Borrow<Entity>,
    I: IntoIterator<Item = E>,
{
    type Out<T: Relation> = Ops<Left, Joins, Right, BreadthFirstTraversal<T, E, I>>;
    fn breadth_first<T: Relation>(self, roots: I) -> Self::Out<T> {
        Ops {
            left: self.left,
            joins: self.joins,
            right: self.right,
            traversal: BreadthFirstTraversal {
                roots,
                _phantom: PhantomData,
            },
        }
    }
}

/// Trait to implement the `join` functionality of the operations API. Any `T` in `join::<T>(query)`
/// must be present in the `Relations<(..)>` parameter of a query. The type of join performed is
/// what's known as an "inner join" which produces permutations of all matched entiteis.
/// The presence of any join operation will make the `for_each` closure 2 arity instead of 1 where
/// the 2nd parameter is a tuple of components from matched entities.
/// See [`BreadthFirst`] for how to perform traversals.
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
/// struct R1;
///
/// impl Relation for R1 {
///     const EXCLUSIVE: bool = false;
/// }
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

impl<Item, Left, Joins, Right, Traversal> Join<Item> for Ops<Left, Joins, Right, Traversal>
where
    Item: for<'a> Joinable<'a, 1>,
    Joins: Append,
    Right: Append,
{
    type Out<T: Relation> =
        Ops<Left, <Joins as Append>::Out<T>, <Right as Append>::Out<Item>, Traversal>;

    fn join<T: Relation>(self, item: Item) -> Self::Out<T> {
        Ops {
            left: self.left,
            joins: PhantomData,
            right: Append::append(self.right, item),
            traversal: self.traversal,
        }
    }
}

/// Control flow enum to allow continue and early return semantics for [`TrappedForEach`] and
/// [`InnerForEach`].
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
///     })
/// }
/// ```
pub enum ControlFlow {
    Continue,
    Exit,
}

impl From<()> for ControlFlow {
    fn from(_: ()) -> Self {
        ControlFlow::Continue
    }
}

/// For each trait to get around lending Iterators not being expressible with current GATs. When
/// traversing diamonds or cycles references to the same entities components can be produced more
/// than once which is why this problem cannot be solved with `unsafe`. So instead it is made so
/// that params provided to the closure cannot escape the closure. Traversing without joins is a
/// common usecase so this variation is distinct from [`InnerForEach`] to allow `1` arity closures.
/// See [`ControlFlow`] for control flow options.
pub trait TrappedForEach {
    type In<'i>;
    fn for_each<Func, Ret>(self, func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: FnMut(Self::In<'_>) -> Ret;
}

impl<Q, R, F, T, E, I> TrappedForEach
    for Ops<&'_ Query<'_, '_, (Q, Relations<R>), F>, (), (), BreadthFirstTraversal<T, E, I>>
where
    Q: WorldQuery,
    R: RelationSet,
    F: ReadOnlyWorldQuery,
    T: Relation,
    E: Borrow<Entity>,
    I: IntoIterator<Item = E>,
{
    type In<'i> = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'i>;
    fn for_each<Func, Ret>(self, mut func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: FnMut(Self::In<'_>) -> Ret,
    {
        let mut queue = self
            .traversal
            .roots
            .into_iter()
            .map(|e| *e.borrow())
            .collect::<VecDeque<Entity>>();

        while let Some(entity) = queue.pop_front() {
            let Ok((left, relations)) = self.left.get(entity) else {
                continue
            };

            for e in relations.edges.edges.iter_fosters::<T>() {
                queue.push_back(e);
            }

            if let ControlFlow::Exit = func(left).into() {
                return;
            }
        }
    }
}

impl<Q, R, F, T, E, I> TrappedForEach
    for Ops<&'_ mut Query<'_, '_, (Q, Relations<R>), F>, (), (), BreadthFirstTraversal<T, E, I>>
where
    Q: WorldQuery,
    R: RelationSet,
    F: ReadOnlyWorldQuery,
    T: Relation,
    E: Borrow<Entity>,
    I: IntoIterator<Item = E>,
{
    type In<'i> = <Q as WorldQuery>::Item<'i>;
    fn for_each<Func, Ret>(self, mut func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: FnMut(Self::In<'_>) -> Ret,
    {
        let mut queue = self
            .traversal
            .roots
            .into_iter()
            .map(|e| *e.borrow())
            .collect::<VecDeque<Entity>>();

        while let Some(entity) = queue.pop_front() {
            let Ok((left, relations)) = self.left.get_mut(entity) else {
                continue
            };

            for e in relations.edges.edges.iter_fosters::<T>() {
                queue.push_back(e);
            }

            if let ControlFlow::Exit = func(left).into() {
                return;
            }
        }
    }
}

/// For each trait to loop through inner joins. Like [`TrappedForEach`] the parameters provided to
/// the closure cannot escape the closure. This will give the caller only matched entities and skip
/// over entities not present in a joined query. Any `.join::<T>(query)` call will result in an
/// innner joined `for_each` call. See [`ControlFlow`] for control flow options.
pub trait InnerForEach<const N: usize> {
    type Left<'l>;
    type Right<'r>;
    fn for_each<Func, Ret>(self, func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'l, 'r> FnMut(&'f mut Self::Left<'l>, Self::Right<'r>) -> Ret;
}

impl<Q, R, F, Joins, Right, const N: usize> InnerForEach<N>
    for Ops<&'_ Query<'_, '_, (Q, Relations<R>), F>, Joins, Right, ()>
where
    Q: WorldQuery,
    R: RelationSet,
    F: ReadOnlyWorldQuery,
    Joins: Product<N>,
    Right: for<'a> Joinable<'a, N>,
{
    type Left<'l> = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'l>;
    type Right<'r> = <Right as Joinable<'r, N>>::Out;
    fn for_each<Func, Ret>(mut self, mut func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'l, 'r> FnMut(&'f mut Self::Left<'l>, Self::Right<'r>) -> Ret,
    {
        for (mut left, relations) in self.left.iter() {
            let mut edge_product = Joins::product(relations.edges);
            let mut matches = [true; N];
            while let Some(entities) = edge_product.advance(matches) {
                matches = Joinable::check(&self.right, entities);

                if matches.iter().any(|b| !b) {
                    continue;
                }

                if let ControlFlow::Exit =
                    func(&mut left, Joinable::join(&mut self.right, entities)).into()
                {
                    return;
                }
            }
        }
    }
}

impl<Q, R, F, Joins, Right, const N: usize> InnerForEach<N>
    for Ops<&'_ mut Query<'_, '_, (Q, Relations<R>), F>, Joins, Right, ()>
where
    Q: WorldQuery,
    R: RelationSet,
    F: ReadOnlyWorldQuery,
    Joins: Product<N>,
    Right: for<'a> Joinable<'a, N>,
{
    type Left<'l> = <Q as WorldQuery>::Item<'l>;
    type Right<'r> = <Right as Joinable<'r, N>>::Out;
    fn for_each<Func, Ret>(mut self, mut func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'l, 'r> FnMut(&'f mut Self::Left<'l>, Self::Right<'r>) -> Ret,
    {
        for (mut left, relations) in self.left.iter_mut() {
            let mut edge_product = Joins::product(relations.edges);
            let mut matches = [true; N];
            while let Some(entities) = edge_product.advance(matches) {
                matches = Joinable::check(&self.right, entities);

                if matches.iter().any(|b| !b) {
                    continue;
                }

                if let ControlFlow::Exit =
                    func(&mut left, Joinable::join(&mut self.right, entities)).into()
                {
                    return;
                }
            }
        }
    }
}

impl<Q, R, F, T, E, I, Joins, Right, const N: usize> InnerForEach<N>
    for Ops<&'_ Query<'_, '_, (Q, Relations<R>), F>, Joins, Right, BreadthFirstTraversal<T, E, I>>
where
    Q: WorldQuery,
    R: RelationSet,
    F: ReadOnlyWorldQuery,
    T: Relation,
    E: Borrow<Entity>,
    I: IntoIterator<Item = E>,
    Joins: Product<N>,
    Right: for<'a> Joinable<'a, N>,
{
    type Left<'l> = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'l>;
    type Right<'r> = <Right as Joinable<'r, N>>::Out;
    fn for_each<Func, Ret>(mut self, mut func: Func)
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

        while let Some(entity) = queue.pop_front() {
            let Ok((mut left, relations)) = self.left.get(entity) else {
                continue
            };

            for e in relations.edges.edges.iter_fosters::<T>() {
                queue.push_back(e);
            }

            let mut edge_product = Joins::product(relations.edges);
            let mut matches = [true; N];

            while let Some(entities) = edge_product.advance(matches) {
                matches = Joinable::check(&self.right, entities);

                if matches.iter().any(|b| !b) {
                    continue;
                }

                if let ControlFlow::Exit =
                    func(&mut left, Joinable::join(&mut self.right, entities)).into()
                {
                    return;
                }
            }
        }
    }
}

impl<Q, R, F, T, E, I, Joins, Right, const N: usize> InnerForEach<N>
    for Ops<
        &'_ mut Query<'_, '_, (Q, Relations<R>), F>,
        Joins,
        Right,
        BreadthFirstTraversal<T, E, I>,
    >
where
    Q: WorldQuery,
    R: RelationSet,
    F: ReadOnlyWorldQuery,
    T: Relation,
    E: Borrow<Entity>,
    I: IntoIterator<Item = E>,
    Joins: Product<N>,
    Right: for<'a> Joinable<'a, N>,
{
    type Left<'l> = <Q as WorldQuery>::Item<'l>;
    type Right<'r> = <Right as Joinable<'r, N>>::Out;
    fn for_each<Func, Ret>(mut self, mut func: Func)
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

        while let Some(entity) = queue.pop_front() {
            let Ok((mut left, relations)) = self.left.get_mut(entity) else {
                continue
            };

            for e in relations.edges.edges.iter_fosters::<T>() {
                queue.push_back(e);
            }

            let mut edge_product = Joins::product(relations.edges);
            let mut matches = [true; N];

            while let Some(entities) = edge_product.advance(matches) {
                matches = Joinable::check(&self.right, entities);

                if matches.iter().any(|b| !b) {
                    continue;
                }

                if let ControlFlow::Exit =
                    func(&mut left, Joinable::join(&mut self.right, entities)).into()
                {
                    return;
                }
            }
        }
    }
}

#[cfg(test)]
#[allow(dead_code)]
#[allow(unused_variables)]
mod compile_tests {
    use super::*;
    use bevy::prelude::*;

    #[derive(Component)]
    struct A;

    #[derive(Component)]
    struct B;

    #[derive(Component)]
    struct C;

    struct R0;

    impl Relation for R0 {}

    struct R1;

    impl Relation for R1 {}

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
            .for_each(|a| {});
    }

    fn breadth_first_immut_joined(left: Query<(&A, Relations<(R0, R1)>)>, right: Query<&B>) {
        left.ops()
            .breadth_first::<R0>(None::<Entity>)
            .join::<R1>(&right)
            .for_each(|a, b| {});
    }

    fn breadth_first_mut(mut left: Query<(&mut A, Relations<(R0, R1)>)>) {
        left.ops_mut()
            .breadth_first::<R0>(None::<Entity>)
            .for_each(|a| {});
    }

    fn breadth_first_mut_joined_mut(
        left: Query<(&A, Relations<(R0, R1)>)>,
        mut right: Query<&mut B>,
    ) {
        left.ops()
            .breadth_first::<R0>(None::<Entity>)
            .join::<R1>(&mut right)
            .for_each(|a, b| {});
    }
}
