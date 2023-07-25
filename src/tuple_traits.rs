use crate::{
    operations::EdgeProduct,
    relation::{EdgeWQItem, Participates, Relation},
};
use bevy::{
    ecs::{
        entity::Entity,
        query::{ReadOnlyWorldQuery, WorldQuery},
        system::Query,
    },
    utils::all_tuples,
};

mod sealed {
    use super::*;
    pub trait Sealed {}

    impl<R: Relation> Sealed for R {}
    impl<R: Relation> Sealed for Option<R> {}

    impl<Q, F> Sealed for &'_ Query<'_, '_, Q, F>
    where
        Q: WorldQuery,
        F: ReadOnlyWorldQuery,
    {
    }

    impl<Q, F> Sealed for &'_ mut Query<'_, '_, Q, F>
    where
        Q: WorldQuery,
        F: ReadOnlyWorldQuery,
    {
    }

    macro_rules! impl_sealed {
        ($($P:ident),*) => {
            impl<$($P: Sealed),*> Sealed for ($($P,)*) {
            }
        };
    }
    all_tuples!(impl_sealed, 1, 16, P);
}

use sealed::*;

macro_rules! count {
    () => { 0 };
    ($_:tt $($tail:tt)*) => { 1  + count!($($tail)*) };
}

pub trait RelationSet: Sized + Sealed {
    type Filters: ReadOnlyWorldQuery;
}

impl<R: Relation> RelationSet for R {
    type Filters = Participates<R>;
}

impl<R: Relation> RelationSet for Option<R> {
    type Filters = ();
}

macro_rules! impl_relation_set {
    ($($P:ident),*) => {
        impl<$($P: Relation),*> RelationSet for ($($P,)*) {
            type Filters = ($(Participates<$P>,)*);
        }
    };
}

all_tuples!(impl_relation_set, 1, 15, P);

pub trait Product<const N: usize> {
    fn product(edges: EdgeWQItem<'_>) -> EdgeProduct<'_, N>;
}

macro_rules! impl_product {
    ($($P:ident),*) => {
        impl<$($P: Relation),*> Product<{ count!($($P )*) }> for ($($P,)*) {
            fn product(edges: EdgeWQItem<'_>) -> EdgeProduct<'_, { count!($($P )*) }> {
                let base_iterators = [$(crate::relation::IterRelations::iter_targets::<$P>(edges.edges),)*];
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

pub trait Joinable<'a, const N: usize>: Sealed {
    type Out;
    fn check(items: &Self, entities: [Entity; N]) -> [bool; N];
    fn join(items: &'a mut Self, entities: [Entity; N]) -> Self::Out;
}

impl<'a, Q, F> Joinable<'a, 1> for &'_ Query<'_, '_, Q, F>
where
    Q: WorldQuery,
    F: ReadOnlyWorldQuery,
{
    type Out = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'a>;

    fn check(items: &Self, [e0]: [Entity; 1]) -> [bool; 1] {
        [items.get(e0).is_ok()]
    }

    fn join(items: &'a mut Self, [e0]: [Entity; 1]) -> Self::Out {
        (**items).get(e0).unwrap()
    }
}

impl<'a, Q, F> Joinable<'a, 1> for &'_ mut Query<'_, '_, Q, F>
where
    Q: WorldQuery,
    F: ReadOnlyWorldQuery,
{
    type Out = <Q as WorldQuery>::Item<'a>;

    fn check(items: &Self, [e0]: [Entity; 1]) -> [bool; 1] {
        [items.get(e0).is_ok()]
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
                $(let [$v] = Joinable::check($p, [$e]); )*
                [$($v,)*]
            }

            fn join(
                ($($p,)*): &'a mut Self,
                [$($e,)*]: [Entity; count!($($P )*)]
            )
                -> Self::Out
            {
                $(let $v = Joinable::join($p, [$e]); )*
                ($($v,)*)
            }
        }
    }
}

all_tuples!(impl_joinable, 2, 15, P, p, e, v);
