use crate::{
    edges::{EdgeIter, EdgesItem},
    relation::{Relation, RelationId, ZstOrPanic},
    tuple_traits::*,
};

use bevy::ecs::{
    component::Component,
    entity::Entity,
    query::{ReadOnlyWorldQuery, WorldQuery},
    system::Query,
};

use std::{borrow::Borrow, marker::PhantomData};

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

/// Struct that is used to track metadata for relation operations.
pub struct Operations<
    Control,
    JoinedTypes = (),
    JoinedQueries = (),
    Traversal = (),
    Starts = (),
    Init = (),
    Fold = (),
> {
    pub(crate) control: Control,
    pub(crate) joined_types: PhantomData<JoinedTypes>,
    pub(crate) joined_queries: JoinedQueries,
    pub(crate) traversal: PhantomData<Traversal>,
    pub(crate) starts: Starts,
    pub(crate) init: Init,
    pub(crate) fold: Fold,
}

/// An extension trait to turn `Query<(X, Relations<R>)>`s into [`Operations`]s which have the
/// trait implementations to build relation operations. This query is called the "control query".
/// The [`RelationSet`] `R` from this query is what is used for joins and traversals any `T` in a
/// subsequent `.join::<T>(_)` or `.traverse::<T>(_)` call must be present in `R`.
/// Also see [`Join`] and [`Traverse`].
pub trait AeryQueryExt {
    /// Provides read only access to the left portion of the [`Query`] tuple.
    fn ops(&self) -> Operations<&Self>;
    /// Provides mutable access to the left portion of the [`Query`] tuple.
    fn ops_mut(&mut self) -> Operations<&mut Self>;
}

impl<'w, 's, Q, F, RS> AeryQueryExt for Query<'w, 's, (Q, Relations<RS>), F>
where
    Q: WorldQuery,
    F: ReadOnlyWorldQuery,
    RS: RelationSet,
{
    #[allow(clippy::let_unit_value)]
    fn ops(&self) -> Operations<&Self> {
        let _ = RS::ZST_OR_PANIC;

        Operations {
            control: self,
            joined_types: PhantomData,
            joined_queries: (),
            traversal: PhantomData,
            starts: (),
            init: (),
            fold: (),
        }
    }

    #[allow(clippy::let_unit_value)]
    fn ops_mut(&mut self) -> Operations<&mut Self> {
        let _ = RS::ZST_OR_PANIC;

        Operations {
            control: self,
            joined_types: PhantomData,
            joined_queries: (),
            traversal: PhantomData,
            starts: (),
            init: (),
            fold: (),
        }
    }
}

pub struct Targets<R>(PhantomData<R>);

pub trait EdgeSide {
    fn entities<'i, 'r, RS>(relations: &'r RelationsItem<'i, RS>) -> EdgeIter<'r>
    where
        'i: 'r,
        RS: RelationSet,
        RelationsItem<'i, RS>: RelationEntries;
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

impl<R: Relation> EdgeSide for Targets<R> {
    fn entities<'i, 'r, RS>(relations: &'r RelationsItem<'i, RS>) -> EdgeIter<'r>
    where
        'i: 'r,
        RS: RelationSet,
        RelationsItem<'i, RS>: RelationEntries,
    {
        relations.targets(RelationId::of::<R>()).iter().copied()
    }
}

/// The traversal functionality of the operations API. Any `T` in `traverse::<T>(roots)` must
/// be present in the [`RelationSet`] of the control query. Diamonds are impossible with `Exclusive`
/// relations where the edges face bottom up instead of top down. For this reason all of Aery's
/// APIs are opinionated with implicit defaults to prefer bottom up edges.
///
/// To descend is to traverse hosts and to ascend is to traverse targets. Descent is breadth first
/// and since relations support multi arity ascent is also breadth first. Ascending exclusive
/// relations is to ascend parents as the "breadth" is always `1`.
///
/// Traversals will not check for cycles or diamonds (possible with multi relations). Cycles will
/// infinite loop and entities may be traversed multiple times for diamonds.
///
/// See [`Join`] for joining queries and:
/// - [`ForEachPermutations`] for operations with just traversals.
/// - [`ForEachPermutations3Arity`] for operations with traversals and joins.
///
/// # Illustration:
/// ```
/// use bevy::prelude::*;
/// use aery::prelude::*;
///
/// #[derive(Component)]
/// struct A;
///
/// #[derive(Relation)]
/// #[cleanup(policy = "Recursive")]
/// struct R;
///
/// #[derive(Relation)]
/// #[cleanup(policy = "Orphan")]
/// struct O;
///
/// fn setup(mut commands: Commands) {
///     commands.add(|wrld: &mut World| {
///         wrld.spawn_empty()
///             .scope::<O>(|parent, ent1| {
///                 ent1.set::<R>(parent)
///                     .scope::<O>(|parent, ent3| {})
///                     .scope::<O>(|parent, ent4| { ent4.set::<R>(parent); });
///             })
///             .scope::<O>(|_, ent2| {
///                 ent2.scope::<R>(|_, ent5| {})
///                     .scope::<R>(|_, ent6| {});
///             });
///     });
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
///     a.ops().traverse::<R>(roots.iter()).for_each(|a_ancestor, a| {
///         // Will traverse in the following order:
///         // (a_ancestor, a) == (0, 1)
///         // (a_ancestor, a) == (0, 2)
///         // (a_ancestor, a) == (1, 3)
///         // (a_ancestor, a) == (1, 4)
///         // (a_ancestor, a) == (2, 5)
///         // (a_ancestor, a) == (2, 6)
///     })
/// }
/// ```
pub trait Traverse<E, I>
where
    E: Borrow<Entity>,
    I: IntoIterator<Item = E>,
{
    type Traversal<T: EdgeSide>;
    fn traverse<T: EdgeSide>(self, starts: I) -> Self::Traversal<T>;
}

impl<Control, JoinedTypes, JoinedQueries, E, Starts> Traverse<E, Starts>
    for Operations<Control, JoinedTypes, JoinedQueries>
where
    E: Borrow<Entity>,
    Starts: IntoIterator<Item = E>,
{
    type Traversal<T: EdgeSide> = Operations<Control, JoinedTypes, JoinedQueries, T, Starts>;

    fn traverse<T: EdgeSide>(self, starts: Starts) -> Self::Traversal<T> {
        Operations {
            control: self.control,
            joined_types: self.joined_types,
            joined_queries: self.joined_queries,
            traversal: PhantomData,
            starts,
            init: self.init,
            fold: self.fold,
        }
    }
}

pub trait FoldBreadth {
    type In<'i>;
    type Out<Init, Fold>;

    fn fold_breadth<Acc, E, Init, Fold>(self, init: Init, fold: Fold) -> Self::Out<Init, Fold>
    where
        Init: FnMut(&mut Self::In<'_>) -> Acc,
        Fold: FnMut(Acc, Self::In<'_>) -> Result<Acc, E>;
}

impl<'a, 'w, 's, Q, RS, F, JoinedTypes, JoinedQueries, Traversal, Starts> FoldBreadth
    for Operations<
        &'a Query<'w, 's, (Q, Relations<RS>), F>,
        JoinedTypes,
        JoinedQueries,
        Traversal,
        Starts,
    >
where
    Q: WorldQuery,
    F: ReadOnlyWorldQuery,
    RS: RelationSet,
{
    type In<'i> = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'i>;
    type Out<Init, Fold> = Operations<
        &'a Query<'w, 's, (Q, Relations<RS>), F>,
        JoinedTypes,
        JoinedQueries,
        Traversal,
        Starts,
        Init,
        Fold,
    >;

    fn fold_breadth<Acc, E, Init, Fold>(self, init: Init, fold: Fold) -> Self::Out<Init, Fold>
    where
        Init: FnMut(&mut Self::In<'_>) -> Acc,
        Fold: FnMut(Acc, Self::In<'_>) -> Result<Acc, E>,
    {
        Operations {
            control: self.control,
            joined_types: self.joined_types,
            joined_queries: self.joined_queries,
            traversal: self.traversal,
            starts: self.starts,
            init,
            fold,
        }
    }
}

impl<'a, 'w, 's, Q, RS, F, JoinedTypes, JoinedQueries, Traversal, Starts> FoldBreadth
    for Operations<
        &'a mut Query<'w, 's, (Q, Relations<RS>), F>,
        JoinedTypes,
        JoinedQueries,
        Traversal,
        Starts,
    >
where
    Q: WorldQuery,
    F: ReadOnlyWorldQuery,
    RS: RelationSet,
{
    type In<'i> = <Q as WorldQuery>::Item<'i>;
    type Out<Init, Fold> = Operations<
        &'a mut Query<'w, 's, (Q, Relations<RS>), F>,
        JoinedTypes,
        JoinedQueries,
        Traversal,
        Starts,
        Init,
        Fold,
    >;

    fn fold_breadth<Acc, E, Init, Fold>(self, init: Init, fold: Fold) -> Self::Out<Init, Fold>
    where
        Init: FnMut(&mut Self::In<'_>) -> Acc,
        Fold: FnMut(Acc, Self::In<'_>) -> Result<Acc, E>,
    {
        Operations {
            control: self.control,
            joined_types: self.joined_types,
            joined_queries: self.joined_queries,
            traversal: self.traversal,
            starts: self.starts,
            init,
            fold,
        }
    }
}

/// The `join` functionality of the operations API. Any `T` in `join::<T>(query)` must be present
/// in the [`RelationSet`] of the control query. The type of join performed is what's known as an
/// "inner join" which produces permutations of all matched entiteis.
///
/// See [`Traverse`] for traversing hierarchies and:
/// - [`ForEachPermutations`] for operations with just joins.
/// - [`ForEachPermutations3Arity`] for operations with joins and traversals.
///
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
/// **then `a.ops().join::<R0>(&b).join::<R1>(&c)`:**
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
    type Joined<T: EdgeSide>;
    fn join<T: EdgeSide>(self, item: Item) -> Self::Joined<T>;
}

impl<Item, Control, JoinedTypes, JoinedQueries, Traversal, Roots> Join<Item>
    for Operations<Control, JoinedTypes, JoinedQueries, Traversal, Roots>
where
    Item: for<'a> Joinable<'a, 1>,
    JoinedTypes: Append,
    JoinedQueries: Append,
{
    type Joined<T: EdgeSide> = Operations<
        Control,
        <JoinedTypes as Append>::Out<T>,
        <JoinedQueries as Append>::Out<Item>,
        Traversal,
        Roots,
    >;

    fn join<T: EdgeSide>(self, item: Item) -> Self::Joined<T> {
        Operations {
            control: self.control,
            joined_types: PhantomData,
            joined_queries: Append::append(self.joined_queries, item),
            traversal: self.traversal,
            starts: self.starts,
            fold: self.fold,
            init: self.init,
        }
    }
}

pub trait Hereditary: Component {}

// TODO: Compile tests for scan_breadth & track
#[cfg(test)]
#[allow(dead_code)]
#[allow(unused_variables)]
mod compile_tests {
    use crate::{self as aery, prelude::*};
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

    fn traverse_immut(left: Query<(&A, Relations<(R0, R1)>)>) {
        left.ops()
            .traverse::<R0>(None::<Entity>)
            .for_each(|a0, a1| {});
    }

    fn traverse_mut(mut left: Query<(&mut A, Relations<(R0, R1)>)>) {
        left.ops_mut()
            .traverse::<R0>(None::<Entity>)
            .for_each(|a0, a1| {});
    }

    /*fn traverse_immut_joined(left: Query<(&A, Relations<(R0, R1)>)>, right: Query<&B>) {
        left.ops()
            .traverse::<R0>(None::<Entity>)
            .join::<R1>(&right)
            .for_each(|a0, a1, b| {});
    }

    fn traverse_mut_joined_mut(left: Query<(&A, Relations<(R0, R1)>)>, mut right: Query<&mut B>) {
        left.ops()
            .traverse::<R0>(None::<Entity>)
            .join::<R1>(&mut right)
            .for_each(|a0, a1, b| {});
    }

    fn query_optional(left: Query<(&A, Relations<(R0, Option<R1>)>)>, b: Query<&B>, c: Query<&C>) {
        left.ops()
            .join::<R0>(&b)
            .join::<R1>(&c)
            .for_each(|a, (b, c)| {});
    }*/
}

/*#[cfg(test)]
mod tests {
    use crate::{self as aery, prelude::*};
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

            world
                .entity_mut(left)
                .set::<R0>(a1)
                .unwrap()
                .set::<R1>(b0)
                .unwrap()
                .set::<R1>(b2)
                .unwrap()
                .set::<R2>(c1);

            world.insert_resource(EntityList { entities });
        }

        fn run(
            left: Query<(&S, Relations<(R0, R1, R2)>)>,
            mut a: Query<&mut A>,
            mut b: Query<&mut B>,
            mut c: Query<&mut C>,
        ) {
            left.ops()
                .join_targets::<R0>(&mut a)
                .join_targets::<R1>(&mut b)
                .join_targets::<R2>(&mut c)
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
            .add_plugins(Aery)
            .add_systems(Update, (init, run, test).chain())
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

            world
                .entity_mut(left)
                .set::<R0>(a0)
                .unwrap()
                .set::<R0>(a2)
                .unwrap()
                .set::<R1>(b1)
                .unwrap()
                .set::<R2>(c0)
                .unwrap()
                .set::<R2>(c2);

            world.insert_resource(EntityList { entities });
        }

        fn run(
            left: Query<(&S, Relations<(R0, R1, R2)>)>,
            mut a: Query<&mut A>,
            mut b: Query<&mut B>,
            mut c: Query<&mut C>,
        ) {
            left.ops()
                .join_targets::<R0>(&mut a)
                .join_targets::<R1>(&mut b)
                .join_targets::<R2>(&mut c)
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
            .add_plugins(Aery)
            .add_systems(Update, (init, run, test).chain())
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

            world
                .entity_mut(left)
                .set::<R0>(a0)
                .unwrap()
                .set::<R0>(a1)
                .unwrap()
                .set::<R0>(a2)
                .unwrap()
                .set::<R1>(b0)
                .unwrap()
                .set::<R1>(b1)
                .unwrap()
                .set::<R1>(b2)
                .unwrap()
                .set::<R2>(c0)
                .unwrap()
                .set::<R2>(c1)
                .unwrap()
                .set::<R2>(c2);

            world.insert_resource(EntityList { entities });
        }

        fn run(
            left: Query<(&S, Relations<(R0, R1, R2)>)>,
            mut a: Query<&mut A>,
            mut b: Query<&mut B>,
            mut c: Query<&mut C>,
        ) {
            left.ops()
                .join_targets::<R0>(&mut a)
                .join_targets::<R1>(&mut b)
                .join_targets::<R2>(&mut c)
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
            .add_plugins(Aery)
            .add_systems(Update, (init, run, test).chain())
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

            world
                .entity_mut(left)
                .set::<R0>(a0)
                .unwrap()
                .set::<R0>(a1)
                .unwrap()
                .set::<R0>(a2)
                .unwrap()
                .set::<R1>(b0)
                .unwrap()
                .set::<R1>(b1)
                .unwrap()
                .set::<R1>(b2)
                .unwrap()
                .set::<R2>(c0)
                .unwrap()
                .set::<R2>(c1)
                .unwrap()
                .set::<R2>(c2);

            world.insert_resource(EntityList { entities });
        }

        fn run(
            left: Query<(&S, Relations<(R0, R1, R2)>)>,
            mut a: Query<&mut A>,
            mut b: Query<&mut B>,
            mut c: Query<&mut C>,
        ) {
            left.ops()
                .join_targets::<R0>(&mut a)
                .join_targets::<R1>(&mut b)
                .join_targets::<R2>(&mut c)
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
            .add_plugins(Aery)
            .add_systems(Update, (init, run, test).chain())
            .run();
    }
}*/
