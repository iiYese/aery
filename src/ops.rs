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

        for i in (1..N).skip(n) {
            self.live_iterators[i] = self.base_iterators[i].clone();
            self.entities[i] = self.live_iterators[i].next();
        }

        'l: {
            for i in (1..N).take(n).rev() {
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

/// Trait to implement the `breadth_first` functionality of the operations API. Any `T` in
/// `breadth_first::<T>(roots)` must be present in the `Relations<(..)>` parameter of a query.
/// Diamonds are impossible with `Exclusive` relations where the edges face bottom up instead of
/// top down. For this reason bottom up graphs are the only pattern that is recognized for
/// traversal. See [`Join`] for performing joins.
/// # Illustration:
/// ```
/// use bevy::prelude::*;
/// use aery::prelude::*;
///
/// #[derive(Component)]
/// struct A;
///
/// struct R;
///
/// impl Relation for R {}
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
///     a.ops().breadth_first::<R>(roots.iter()).for_each(|a| {
///         // Will traverse in the following order:
///         // 0 -> 1 -> 2 -> 3 -> 4 -> 5 -> 6
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
/// struct R0;
///
/// impl Relation for R0 {}
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
/// struct R;
///
/// impl Relation for R {}
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
///         // () impls Into<ControlFlow> for convenience. Return types still need to be the same
///         // so providing this is nessecary when doing any controlflow manipulation.
///         ControlFlow::Continue
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
            let mut matches = [false; N];
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
            let mut matches = [false; N];
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
            let mut matches = [false; N];

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
            let mut matches = [false; N];

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

    struct R0;

    impl Relation for R0 {
        const EXCLUSIVE: bool = false;
    }

    struct R1;

    impl Relation for R1 {
        const EXCLUSIVE: bool = false;
    }

    struct R2;

    impl Relation for R2 {
        const EXCLUSIVE: bool = false;
    }

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
