use crate::{
    relation::{CheckRelations, EdgeIter, EdgeWQ, EdgeWQItem, IterRelations, Relation, ZstOrPanic},
    tuple_traits::*,
};

use bevy::ecs::{
    entity::Entity,
    query::{ReadOnlyWorldQuery, WorldQuery},
    system::Query,
};

use std::{borrow::Borrow, marker::PhantomData};

mod for_each;
mod for_each_3arity;
mod for_each_4arity;
mod for_each_5arity;

pub use for_each::*;
pub use for_each_3arity::*;
pub use for_each_4arity::*;
pub use for_each_5arity::*;

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
    pub(crate) entity: Entity,
    _filters: R::Filters,
    _phantom: PhantomData<R>,
}

impl<R: RelationSet> CheckRelations for RelationsItem<'_, R> {
    fn has_host(
        &self,
        relation: impl Into<crate::Var<crate::relation::RelationId>>,
        host: impl Into<crate::Var<Entity>>,
    ) -> bool {
        self.edges.edges.has_host(relation, host)
    }

    fn has_target(
        &self,
        relation: impl Into<crate::Var<crate::relation::RelationId>>,
        target: impl Into<crate::Var<Entity>>,
    ) -> bool {
        self.edges.edges.has_target(relation, target)
    }
}

impl<R: RelationSet> CheckRelations for Option<&RelationsItem<'_, R>> {
    fn has_host(
        &self,
        relation: impl Into<crate::Var<crate::relation::RelationId>>,
        host: impl Into<crate::Var<Entity>>,
    ) -> bool {
        self.is_some_and(|item| item.has_host(relation, host))
    }

    fn has_target(
        &self,
        relation: impl Into<crate::Var<crate::relation::RelationId>>,
        target: impl Into<crate::Var<Entity>>,
    ) -> bool {
        self.is_some_and(|item: &RelationsItem<'_, R>| item.has_target(relation, target))
    }
}

impl<RS: RelationSet> IterRelations for RelationsItem<'_, RS> {
    type Entities<'a> = EdgeIter<'a>
    where
        Self: 'a;

    fn iter_hosts<R: Relation>(&self) -> Self::Entities<'_> {
        self.edges.edges.iter_hosts::<R>()
    }

    fn iter_targets<R: Relation>(&self) -> Self::Entities<'_> {
        self.edges.edges.iter_targets::<R>()
    }
}

impl<RS: RelationSet> IterRelations for Option<&RelationsItem<'_, RS>> {
    type Entities<'a> = std::iter::Flatten<std::option::IntoIter<EdgeIter<'a>>>
    where
        Self: 'a;

    fn iter_hosts<R: Relation>(&self) -> Self::Entities<'_> {
        self.map(|relations| relations.iter_hosts::<R>())
            .into_iter()
            .flatten()
    }

    fn iter_targets<R: Relation>(&self) -> Self::Entities<'_> {
        self.map(|relations| relations.iter_targets::<R>())
            .into_iter()
            .flatten()
    }
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
    control: Control,
    joined_types: PhantomData<JoinedTypes>,
    joined_queries: JoinedQueries,
    traversal: PhantomData<Traversal>,
    starts: Starts,
    init: Init,
    fold: Fold,
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

impl<'w, 's, Q, F, R> AeryQueryExt for Query<'w, 's, (Q, Relations<R>), F>
where
    Q: WorldQuery,
    F: ReadOnlyWorldQuery,
    R: RelationSet + Send + Sync,
{
    #[allow(clippy::let_unit_value)]
    fn ops(&self) -> Operations<&Self> {
        let _ = R::ZST_OR_PANIC;

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
        let _ = R::ZST_OR_PANIC;

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

pub trait EdgeQuery {
    fn entities<'a>(edges: &EdgeWQItem<'a>) -> EdgeIter<'a>;
}

impl<R: Relation> EdgeQuery for R {
    fn entities<'a>(edges: &EdgeWQItem<'a>) -> EdgeIter<'a> {
        edges.edges.iter_hosts::<R>()
    }
}

pub struct Targets<R: Relation>(PhantomData<R>);

impl<R: Relation> EdgeQuery for Targets<R> {
    fn entities<'a>(edges: &EdgeWQItem<'a>) -> EdgeIter<'a> {
        edges.edges.iter_targets::<R>()
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
    type Traversal<T: Relation>;
    type TargetTraversal<T: Relation>;
    fn traverse<T: Relation>(self, starts: I) -> Self::Traversal<T>;
    fn traverse_targets<T: Relation>(self, starts: I) -> Self::TargetTraversal<T>;
}

impl<Control, JoinedTypes, JoinedQueries, E, Starts> Traverse<E, Starts>
    for Operations<Control, JoinedTypes, JoinedQueries>
where
    E: Borrow<Entity>,
    Starts: IntoIterator<Item = E>,
{
    type Traversal<T: Relation> = Operations<Control, JoinedTypes, JoinedQueries, T, Starts>;

    type TargetTraversal<T: Relation> =
        Operations<Control, JoinedTypes, JoinedQueries, Targets<T>, Starts>;

    fn traverse<T: Relation>(self, starts: Starts) -> Self::Traversal<T> {
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

    fn traverse_targets<T: Relation>(self, starts: Starts) -> Self::TargetTraversal<T> {
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

impl<'a, 'w, 's, Q, R, F, JoinedTypes, JoinedQueries, Traversal, Starts> FoldBreadth
    for Operations<
        &'a Query<'w, 's, (Q, Relations<R>), F>,
        JoinedTypes,
        JoinedQueries,
        Traversal,
        Starts,
    >
where
    Q: WorldQuery,
    R: RelationSet,
    F: ReadOnlyWorldQuery,
{
    type In<'i> = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'i>;
    type Out<Init, Fold> = Operations<
        &'a Query<'w, 's, (Q, Relations<R>), F>,
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

impl<'a, 'w, 's, Q, R, F, JoinedTypes, JoinedQueries, Traversal, Starts> FoldBreadth
    for Operations<
        &'a mut Query<'w, 's, (Q, Relations<R>), F>,
        JoinedTypes,
        JoinedQueries,
        Traversal,
        Starts,
    >
where
    Q: WorldQuery,
    R: RelationSet,
    F: ReadOnlyWorldQuery,
{
    type In<'i> = <Q as WorldQuery>::Item<'i>;
    type Out<Init, Fold> = Operations<
        &'a mut Query<'w, 's, (Q, Relations<R>), F>,
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
    type Joined<T: Relation>;
    type TargetJoined<T: Relation>;
    fn join<T: Relation>(self, item: Item) -> Self::Joined<T>;
    fn join_targets<T: Relation>(self, item: Item) -> Self::TargetJoined<T>;
}

impl<Item, Control, JoinedTypes, JoinedQueries, Traversal, Roots> Join<Item>
    for Operations<Control, JoinedTypes, JoinedQueries, Traversal, Roots>
where
    Item: for<'a> Joinable<'a, 1>,
    JoinedTypes: Append,
    JoinedQueries: Append,
{
    type Joined<T: Relation> = Operations<
        Control,
        <JoinedTypes as Append>::Out<T>,
        <JoinedQueries as Append>::Out<Item>,
        Traversal,
        Roots,
    >;

    type TargetJoined<T: Relation> = Operations<
        Control,
        <JoinedTypes as Append>::Out<Targets<T>>,
        <JoinedQueries as Append>::Out<Item>,
        Traversal,
        Roots,
    >;

    fn join<T: Relation>(self, item: Item) -> Self::Joined<T> {
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

    fn join_targets<T: Relation>(self, item: Item) -> Self::TargetJoined<T> {
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

    fn traverse_immut_joined(left: Query<(&A, Relations<(R0, R1)>)>, right: Query<&B>) {
        left.ops()
            .traverse::<R0>(None::<Entity>)
            .join::<R1>(&right)
            .for_each(|a0, a1, b| {});
    }

    fn traverse_mut(mut left: Query<(&mut A, Relations<(R0, R1)>)>) {
        left.ops_mut()
            .traverse::<R0>(None::<Entity>)
            .for_each(|a0, a1| {});
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
    }
}

#[cfg(test)]
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
}
