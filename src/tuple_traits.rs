use crate::{
    edges::{EdgeInfo, Edges, HierarchyEdges},
    operations::utils::{EdgeProduct, EdgeSide, RelationsItem},
    relation::{Hierarchy, Relation, RelationId},
};
use core::any::TypeId;

use bevy_ecs::{
    entity::Entity,
    query::{QueryData, QueryFilter, ReadOnlyQueryData, WorldQuery},
    system::Query,
};
use bevy_utils::all_tuples;

mod sealed {
    use super::*;
    pub trait Sealed {}

    impl Sealed for Hierarchy {}
    impl<R: Relation> Sealed for R {}
    impl<R: Relation> Sealed for Option<R> {}

    impl<D, F> Sealed for &'_ Query<'_, '_, D, F>
    where
        D: QueryData,
        F: QueryFilter,
    {
    }

    impl<D, F> Sealed for &'_ mut Query<'_, '_, D, F>
    where
        D: QueryData,
        F: QueryFilter,
    {
    }

    macro_rules! impl_sealed {
        ($($P:ident),*) => {
            impl<$($P: Sealed),*> Sealed for ($($P,)*) {}
        };
    }

    all_tuples!(impl_sealed, 1, 16, P);
}

use sealed::*;

macro_rules! count {
    () => { 0 };
    ($_:tt $($tail:tt)*) => { 1  + count!($($tail)*) };
}

#[doc(hidden)]
pub trait Append {
    type Out<Item>;
    fn append<Item>(tup: Self, item: Item) -> Self::Out<Item>;
}

impl Append for () {
    type Out<Item> = (Item,);
    fn append<Item>(_: Self, item: Item) -> Self::Out<Item> {
        (item,)
    }
}

macro_rules! impl_append {
    ($(($P:ident, $p:ident)),*) => {
        impl<$($P),*> Append for ($($P,)*) {
            type Out<Item> = ($($P),*, Item);
            fn append<Item>(($($p,)*): Self, item: Item) -> Self::Out<Item> {
                ($($p),*, item)
            }
        }
    }
}

all_tuples!(impl_append, 1, 15, P, p);

#[doc(hidden)]
pub trait RelationSet: Sized + Sealed {
    type Edges: ReadOnlyQueryData;
    type Types: 'static;
}

impl RelationSet for Hierarchy {
    type Edges = HierarchyEdges;
    type Types = Hierarchy;
}

impl<R: Relation> RelationSet for R {
    type Edges = Edges<R>;
    type Types = R;
}

impl<R: Relation> RelationSet for Option<R> {
    type Edges = Option<Edges<R>>;
    type Types = R;
}

macro_rules! impl_relation_set {
    ($($P:ident),*) => {
        impl<$($P: RelationSet),*> RelationSet for ($($P,)*) {
            type Edges = ($($P::Edges,)*);
            type Types = ($($P::Types,)*);
        }
    };
}

all_tuples!(impl_relation_set, 1, 15, P);

/// Get information from multiple edge buckets.
pub trait RelationEntries {
    /// Get all hosts of a relation type.
    fn hosts(&self, id: impl Into<RelationId>) -> &[Entity];
    /// Get all targets of a relation type.
    fn targets(&self, id: impl Into<RelationId>) -> &[Entity];
}

impl RelationEntries for RelationsItem<'_, Hierarchy> {
    fn hosts(&self, id: impl Into<RelationId>) -> &[Entity] {
        if TypeId::of::<Hierarchy>() == id.into().0 {
            self.edges.hosts()
        } else {
            &[]
        }
    }

    fn targets(&self, id: impl Into<RelationId>) -> &[Entity] {
        if TypeId::of::<Hierarchy>() == id.into().0 {
            self.edges.targets()
        } else {
            &[]
        }
    }
}

impl<R: Relation> RelationEntries for RelationsItem<'_, R> {
    fn hosts(&self, id: impl Into<RelationId>) -> &[Entity] {
        if TypeId::of::<R>() == id.into().0 {
            self.edges.hosts()
        } else {
            &[]
        }
    }

    fn targets(&self, id: impl Into<RelationId>) -> &[Entity] {
        if TypeId::of::<R>() == id.into().0 {
            self.edges.targets()
        } else {
            &[]
        }
    }
}

impl<R: Relation> RelationEntries for RelationsItem<'_, Option<R>> {
    fn hosts(&self, id: impl Into<RelationId>) -> &[Entity] {
        if TypeId::of::<R>() == id.into().0 {
            self.edges.hosts()
        } else {
            &[]
        }
    }

    fn targets(&self, id: impl Into<RelationId>) -> &[Entity] {
        if TypeId::of::<R>() == id.into().0 {
            self.edges.targets()
        } else {
            &[]
        }
    }
}

macro_rules! impl_relation_entries {
    ($(($P:ident, $e:ident)),*) => {
        impl<$($P: RelationSet),*> RelationEntries for RelationsItem<'_, ($($P,)*)>
        where
            $(for<'a> <$P::Edges as WorldQuery>::Item<'a>: EdgeInfo,)*
        {
            fn hosts(&self, id: impl Into<RelationId>) -> &[Entity] {
                let id = id.into();
                let ($($e,)*) = &self.edges;
                $(if TypeId::of::<$P::Types>() == id.0 { return $e.hosts() })*
                &[]
            }

            fn targets(&self, id: impl Into<RelationId>) -> &[Entity] {
                let id = id.into();
                let ($($e,)*) = &self.edges;
                $(if TypeId::of::<$P::Types>() == id.0 { return $e.targets() })*
                &[]
            }
        }
    }
}

all_tuples!(impl_relation_entries, 1, 15, P, e);

#[doc(hidden)]
pub trait Product<const N: usize> {
    fn product<'i, 'r, RS>(relations: &'r RelationsItem<'i, RS>) -> EdgeProduct<'r, N>
    where
        'i: 'r,
        RS: RelationSet,
        RelationsItem<'i, RS>: RelationEntries;
}

macro_rules! impl_product {
    ($($P:ident),*) => {
        impl<$($P: EdgeSide),*> Product<{ count!($($P )*) }> for ($($P,)*) {
            fn product<'i, 'r, RS>(relations: &'r RelationsItem<'i, RS>)
                -> EdgeProduct<'r, { count!($($P )*) }>
            where
                'i: 'r,
                RS: RelationSet,
                RelationsItem<'i, RS>: RelationEntries
            {
                let base_iterators = [$(<$P as EdgeSide>::entities(&relations),)*];
                let live_iterators = base_iterators.clone();
                let entities = [None::<Entity>; count!($($P )*)];

                EdgeProduct {
                    base_iterators,
                    live_iterators,
                    entities,
                }
            }
        }
    };
}

all_tuples!(impl_product, 1, 15, P);

#[doc(hidden)]
pub trait Joinable<'a, const N: usize>: Sealed {
    type Out;
    fn check(items: &Self, entities: [Entity; N]) -> [bool; N];
    fn join(items: &'a mut Self, entities: [Entity; N]) -> Self::Out;
}

impl<'a, D, F> Joinable<'a, 1> for &'_ Query<'_, '_, D, F>
where
    D: QueryData,
    F: QueryFilter,
{
    type Out = <D::ReadOnly as WorldQuery>::Item<'a>;

    fn check(items: &Self, [e0]: [Entity; 1]) -> [bool; 1] {
        [items.contains(e0)]
    }

    fn join(items: &'a mut Self, [e0]: [Entity; 1]) -> Self::Out {
        (**items).get(e0).unwrap()
    }
}

impl<'a, D, F> Joinable<'a, 1> for &'_ mut Query<'_, '_, D, F>
where
    D: QueryData,
    F: QueryFilter,
{
    type Out = <D as WorldQuery>::Item<'a>;

    fn check(items: &Self, [e0]: [Entity; 1]) -> [bool; 1] {
        [items.contains(e0)]
    }

    fn join(items: &'a mut Self, [e0]: [Entity; 1]) -> Self::Out {
        (**items).get_mut(e0).unwrap()
    }
}

impl<'a, P0> Joinable<'a, 1> for (P0,)
where
    P0: Sealed + Joinable<'a, 1>,
{
    type Out = <P0 as Joinable<'a, 1>>::Out;

    fn check((p0,): &Self, [e0]: [Entity; 1]) -> [bool; 1] {
        Joinable::check(p0, [e0])
    }

    fn join((p0,): &'a mut Self, [e0]: [Entity; 1]) -> Self::Out {
        Joinable::join(p0, [e0])
    }
}

macro_rules! impl_joinable {
    ($(($P:ident, $p:ident, $e:ident, $v:ident)),*) => {
        impl<'a, $($P),*> Joinable<'a, { count!($($P )*) }> for ($($P,)*)
        where
            $($P: Joinable<'a, 1>,)*
        {
            type Out = ($(<$P as Joinable<'a, 1>>::Out,)*);

            fn check(
                ($($p,)*): &Self,
                [$($e,)*]: [Entity; count!($($P )*)]
            )
                -> [bool; count!($($p )*)]
            {
                $(let [$v] = Joinable::check($p, [$e]);)*
                [$($v,)*]
            }

            fn join(
                ($($p,)*): &'a mut Self,
                [$($e,)*]: [Entity; count!($($P )*)]
            )
                -> Self::Out
            {
                $(let $v = Joinable::join($p, [$e]);)*
                ($($v,)*)
            }
        }
    }
}

all_tuples!(impl_joinable, 2, 15, P, p, e, v);

#[doc(hidden)]
pub trait Trackable<'a, const N: usize>: Sealed {
    type Out;
    fn update(items: &Self, entity: Entity, fallback: [Entity; N]) -> [Entity; N];
    fn retrieve(items: &'a mut Self, entities: [Entity; N]) -> Option<Self::Out>;
}

impl<'a, D, F> Trackable<'a, 1> for &'_ Query<'_, '_, D, F>
where
    D: QueryData,
    F: QueryFilter,
{
    type Out = <D::ReadOnly as WorldQuery>::Item<'a>;

    fn update(items: &Self, entity: Entity, [fallback]: [Entity; 1]) -> [Entity; 1] {
        [if items.contains(entity) {
            entity
        } else {
            fallback
        }]
    }

    fn retrieve(items: &'a mut Self, [e0]: [Entity; 1]) -> Option<Self::Out> {
        items.get(e0).ok()
    }
}

impl<'a, D, F> Trackable<'a, 1> for &'_ mut Query<'_, '_, D, F>
where
    D: QueryData,
    F: QueryFilter,
{
    type Out = <D as WorldQuery>::Item<'a>;

    fn update(items: &Self, entity: Entity, [fallback]: [Entity; 1]) -> [Entity; 1] {
        [if items.contains(entity) {
            entity
        } else {
            fallback
        }]
    }

    fn retrieve(items: &'a mut Self, [e0]: [Entity; 1]) -> Option<Self::Out> {
        items.get_mut(e0).ok()
    }
}

impl<'a, P0> Trackable<'a, 1> for (P0,)
where
    P0: Sealed + Trackable<'a, 1>,
{
    type Out = <P0 as Trackable<'a, 1>>::Out;

    fn update((p0,): &Self, entity: Entity, [e0]: [Entity; 1]) -> [Entity; 1] {
        Trackable::update(p0, entity, [e0])
    }

    fn retrieve((p0,): &'a mut Self, [e0]: [Entity; 1]) -> Option<Self::Out> {
        Trackable::retrieve(p0, [e0])
    }
}

macro_rules! impl_trackable {
    ($(($P:ident, $p:ident, $e:ident, $v:ident)),*) => {
        impl<'a, $($P),*> Trackable<'a, { count!($($P )*) }> for ($($P,)*)
        where
            $($P: Trackable<'a, 1>,)*
        {
            type Out = ($(<$P as Trackable<'a, 1>>::Out,)*);

            fn update(
                ($($p,)*): &Self,
                entity: Entity,
                [$($e,)*]: [Entity; count!($($P )*)]
            )
                -> [Entity; count!($($p )*)]
            {
                $(let [$v] = Trackable::update($p, entity, [$e]);)*
                [$($v,)*]
            }

            fn retrieve(
                ($($p,)*): &'a mut Self,
                [$($e,)*]: [Entity; count!($($P )*)]
            )
                -> Option<Self::Out>
            {
                $(let $v = Trackable::retrieve($p, [$e])?;)*
                Some(($($v,)*))
            }
        }
    }
}

all_tuples!(impl_trackable, 2, 15, P, p, e, v);
