use crate::{
    relation::{CheckRelations, EdgeWQ, EdgeWQItem, Relation, ZstOrPanic},
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

impl<RS: RelationSet> RelationsItem<'_, RS> {
    pub fn iter_hosts<R: Relation>(&self) -> EdgeIter<'_> {
        let _ = R::ZST_OR_PANIC;
        self.edges.edges.iter_hosts::<R>()
    }

    pub fn iter_targets<R: Relation>(&self) -> EdgeIter<'_> {
        let _ = R::ZST_OR_PANIC;
        self.edges.edges.iter_targets::<R>()
    }
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
    traversal: Traversal,
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
            traversal: (),
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
            traversal: (),
            starts: (),
            init: (),
            fold: (),
        }
    }
}

pub trait Traversal {
    fn iter<'a>(edges: &EdgeWQItem<'a>) -> EdgeIter<'a>;
}

pub struct BreadthFirstAscent<R: Relation>(PhantomData<R>);
pub struct BreadthFirstDescent<R: Relation>(PhantomData<R>);

impl<R: Relation> Traversal for BreadthFirstAscent<R> {
    fn iter<'a>(edges: &EdgeWQItem<'a>) -> EdgeIter<'a> {
        edges.edges.iter_targets::<R>()
    }
}

impl<R: Relation> Traversal for BreadthFirstDescent<R> {
    fn iter<'a>(edges: &EdgeWQItem<'a>) -> EdgeIter<'a> {
        edges.edges.iter_hosts::<R>()
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
    type Out<T: Relation>;
    type OutDown<T: Relation>;

    fn traverse<T: Relation>(self, starts: I) -> Self::OutDown<T>;
    fn traverse_targets<T: Relation>(self, starts: I) -> Self::Out<T>;
}

impl<Control, JoinedTypes, JoinedQueries, E, Starts, Init, Fold> Traverse<E, Starts>
    for Operations<Control, JoinedTypes, JoinedQueries, (), (), Init, Fold>
where
    E: Borrow<Entity>,
    Starts: IntoIterator<Item = E>,
{
    type Out<T: Relation> =
        Operations<Control, JoinedTypes, JoinedQueries, BreadthFirstAscent<T>, Starts, Init, Fold>;

    type OutDown<T: Relation> =
        Operations<Control, JoinedTypes, JoinedQueries, BreadthFirstDescent<T>, Starts, Init, Fold>;

    fn traverse<T: Relation>(self, starts: Starts) -> Self::OutDown<T> {
        Operations {
            control: self.control,
            joined_types: self.joined_types,
            joined_queries: self.joined_queries,
            traversal: BreadthFirstDescent::<T>(PhantomData),
            init: self.init,
            fold: self.fold,
            starts,
        }
    }

    fn traverse_targets<T: Relation>(self, starts: Starts) -> Self::Out<T> {
        Operations {
            control: self.control,
            joined_types: self.joined_types,
            joined_queries: self.joined_queries,
            traversal: BreadthFirstAscent::<T>(PhantomData),
            init: self.init,
            fold: self.fold,
            starts,
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
    type Out<T: Relation>;
    fn join<T: Relation>(self, item: Item) -> Self::Out<T>;
}

impl<Item, Control, JoinedTypes, JoinedQueries, Traversal, Roots> Join<Item>
    for Operations<Control, JoinedTypes, JoinedQueries, Traversal, Roots>
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
        Roots,
    >;

    fn join<T: Relation>(self, item: Item) -> Self::Out<T> {
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
/// E8:::pink --R--> E3
/// E9:::pink --R--> E3
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
}

impl From<()> for ControlFlow {
    fn from(_: ()) -> Self {
        ControlFlow::Continue
    }
}

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
    for Operations<&'_ Query<'_, '_, (Q, Relations<R>), F>, (), (), T, I>
where
    Q: WorldQuery,
    R: RelationSet,
    F: ReadOnlyWorldQuery,
    T: Traversal,
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
            .starts
            .into_iter()
            .map(|e| *e.borrow())
            .collect::<VecDeque<Entity>>();

        'queue: while let Some(entity) = queue.pop_front() {
            let Ok((mut control, relations)) = self.control.get(entity) else {
                continue
            };

            for e in T::iter(&relations.edges) {
                let Ok(traversal_item) = self.control.get(e) else {
                    continue
                };

                match func(&mut control, traversal_item.0).into() {
                    ControlFlow::Exit => return,
                    ControlFlow::Probe => {
                        queue.clear();
                        queue.push_back(e);
                        continue 'queue;
                    }
                    _ => {}
                }
            }

            for e in T::iter(&relations.edges) {
                queue.push_back(e);
            }
        }
    }
}

impl<Q, R, F, T, E, I> ForEachPermutations<0>
    for Operations<&'_ mut Query<'_, '_, (Q, Relations<R>), F>, (), (), T, I>
where
    Q: WorldQuery,
    R: RelationSet,
    F: ReadOnlyWorldQuery,
    T: Traversal,
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
            .starts
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

            for e in T::iter(&relations.edges) {
                // SAFETY: Self referential relations are impossible so this is always safe.
                let Ok(traversal_item) = (unsafe { self.control.get_unchecked(e) }) else {
                    continue
                };

                match func(&mut control, traversal_item.0).into() {
                    ControlFlow::Exit => return,
                    ControlFlow::Probe => {
                        queue.clear();
                        queue.push_back(e);
                        continue 'queue;
                    }
                    _ => {}
                }
            }

            for e in T::iter(&relations.edges) {
                queue.push_back(e);
            }
        }
    }
}

impl<Q, R, F, T, E, I, Acc, Err, Init, Fold> ForEachPermutations<0>
    for Operations<&'_ Query<'_, '_, (Q, Relations<R>), F>, (), (), T, I, Init, Fold>
where
    Q: WorldQuery,
    R: RelationSet,
    F: ReadOnlyWorldQuery,
    T: Traversal,
    E: Borrow<Entity>,
    I: IntoIterator<Item = E>,
    Init: for<'a> FnMut(&mut <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'a>) -> Acc,
    Fold: for<'a> FnMut(
        Acc,
        <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'a>,
    ) -> Result<Acc, Err>,
{
    type Left<'l> = (
        <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'l>,
        Result<Acc, Err>,
    );
    type Right<'r> = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'r>;

    fn for_each<Func, Ret>(mut self, mut func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'l, 'r> FnMut(&'f mut Self::Left<'l>, Self::Right<'r>) -> Ret,
    {
        let mut queue = self
            .starts
            .into_iter()
            .map(|e| *e.borrow())
            .collect::<VecDeque<Entity>>();

        'queue: while let Some(entity) = queue.pop_front() {
            let Ok((mut control, relations)) = self.control.get(entity) else {
                continue
            };

            let mut acc = Ok::<_, Err>((self.init)(&mut control));

            for e in T::iter(&relations.edges) {
                let Ok(traversal_item) = self.control.get(e) else {
                    continue
                };

                let Ok(acc_ok) = acc else {
                    break;
                };

                acc = (self.fold)(acc_ok, traversal_item.0);
            }

            let mut left = (control, acc);

            for e in T::iter(&relations.edges) {
                let Ok(traversal_item) = self.control.get(e) else {
                    continue
                };

                match func(&mut left, traversal_item.0).into() {
                    ControlFlow::Exit => return,
                    ControlFlow::Probe => {
                        queue.clear();
                        queue.push_back(e);
                        continue 'queue;
                    }
                    _ => {}
                }
            }

            for e in T::iter(&relations.edges) {
                queue.push_back(e);
            }
        }
    }
}

impl<Q, R, F, T, E, I, Acc, Err, Init, Fold> ForEachPermutations<0>
    for Operations<&'_ mut Query<'_, '_, (Q, Relations<R>), F>, (), (), T, I, Init, Fold>
where
    Q: WorldQuery,
    R: RelationSet,
    F: ReadOnlyWorldQuery,
    T: Traversal,
    E: Borrow<Entity>,
    I: IntoIterator<Item = E>,
    Init: for<'a> FnMut(&mut <Q as WorldQuery>::Item<'a>) -> Acc,
    Fold: for<'a> FnMut(Acc, <Q as WorldQuery>::Item<'a>) -> Result<Acc, Err>,
{
    type Left<'l> = (<Q as WorldQuery>::Item<'l>, Result<Acc, Err>);
    type Right<'r> = <Q as WorldQuery>::Item<'r>;

    fn for_each<Func, Ret>(mut self, mut func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'l, 'r> FnMut(&'f mut Self::Left<'l>, Self::Right<'r>) -> Ret,
    {
        let mut queue = self
            .starts
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

            let mut acc = Ok::<_, Err>((self.init)(&mut control));

            for e in T::iter(&relations.edges) {
                // SAFETY: Self referential relations are impossible so this is always safe.
                let Ok(traversal_item) = (unsafe { self.control.get_unchecked(e) }) else {
                    continue
                };

                let Ok(acc_ok) = acc else {
                    break;
                };

                acc = (self.fold)(acc_ok, traversal_item.0);
            }

            let mut left = (control, acc);

            for e in T::iter(&relations.edges) {
                // SAFETY: Self referential relations are impossible so this is always safe.
                let Ok(traversal_item) = (unsafe { self.control.get_unchecked(e) }) else {
                    continue
                };

                match func(&mut left, traversal_item.0).into() {
                    ControlFlow::Exit => return,
                    ControlFlow::Probe => {
                        queue.clear();
                        queue.push_back(e);
                        continue 'queue;
                    }
                    _ => {}
                }
            }

            for e in T::iter(&relations.edges) {
                queue.push_back(e);
            }
        }
    }
}

/// A 3 arity version of [`ForEachPermutations`] for when operations feature a traversal with 1 or
/// more joins. Will iterate through hierarchy permutations and join permutations together.
/// - The left and middle paramaters will be an ancestor/descendant pairs.
/// - The rightmost parameter will be a tuple of all the query fetch parameters from joined queries
/// where the entity being joined on is the same entity that is the middle parameter. This is the
/// ancestor or descendant depending on if the traversal is an ascent or descent.
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
    for Operations<&'_ Query<'_, '_, (Q, Relations<R>), F>, JoinedTypes, JoinedQueries, T, I>
where
    Q: WorldQuery,
    R: RelationSet,
    F: ReadOnlyWorldQuery,
    T: Traversal,
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
            .starts
            .into_iter()
            .map(|e| *e.borrow())
            .collect::<VecDeque<Entity>>();

        'queue: while let Some(entity) = queue.pop_front() {
            let Ok((mut left_components, left_edges)) = self.control.get(entity) else {
                continue
            };

            for mid in T::iter(&left_edges.edges) {
                let Ok((mut mid_components, mid_edges)) = self
                    .control
                    .get(mid)
                else {
                    continue
                };

                let mut edge_product = JoinedTypes::product(mid_edges.edges);
                let mut matches = [false; N];

                while let Some(entities) = edge_product.advance(matches) {
                    matches = Joinable::check(&self.joined_queries, entities);

                    if matches.iter().any(|b| !b) {
                        continue;
                    }

                    match func(
                        &mut left_components,
                        &mut mid_components,
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
                            queue.push_back(mid);
                            continue 'queue;
                        }
                        _ => {}
                    }
                }
            }

            for e in T::iter(&left_edges.edges) {
                queue.push_back(e);
            }
        }
    }
}

impl<Q, R, F, T, E, I, JoinedTypes, JoinedQueries, const N: usize> ForEachPermutations3Arity<N>
    for Operations<&'_ mut Query<'_, '_, (Q, Relations<R>), F>, JoinedTypes, JoinedQueries, T, I>
where
    Q: WorldQuery,
    R: RelationSet,
    F: ReadOnlyWorldQuery,
    T: Traversal,
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
            .starts
            .into_iter()
            .map(|e| *e.borrow())
            .collect::<VecDeque<Entity>>();

        'queue: while let Some(entity) = queue.pop_front() {
            // SAFETY: Self referential relations are impossible so this is always safe.
            let Ok((mut left_components, left_edges)) = (unsafe {
                self.control.get_unchecked(entity)
            }) else {
                continue
            };

            for mid in T::iter(&left_edges.edges) {
                // SAFETY: Self referential relations are impossible so this is always safe.
                let Ok((mut mid_components, mid_edges)) = (unsafe {
                    self.control.get_unchecked(mid)
                }) else {
                    continue
                };

                let mut edge_product = JoinedTypes::product(mid_edges.edges);
                let mut matches = [false; N];

                while let Some(entities) = edge_product.advance(matches) {
                    matches = Joinable::check(&self.joined_queries, entities);

                    if matches.iter().any(|b| !b) {
                        continue;
                    }

                    match func(
                        &mut left_components,
                        &mut mid_components,
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
                            queue.push_back(mid);
                            continue 'queue;
                        }
                        _ => {}
                    }
                }
            }

            for e in T::iter(&left_edges.edges) {
                queue.push_back(e);
            }
        }
    }
}

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
            .add_plugins(Aery)
            .add_systems(Update, (init, run, test).chain())
            .run();
    }
}
