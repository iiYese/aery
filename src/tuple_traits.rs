use crate::{
    edges::{EdgeInfo, Edges, EdgesItem},
    operations::{EdgeProduct, RelationsItem},
    relation::{Relation, RelationId},
};
use core::any::TypeId;

use seq_macro::seq;

use bevy::{
    ecs::{
        component::Component,
        entity::Entity,
        query::{ReadOnlyWorldQuery, WorldQuery},
        system::Query,
    },
    utils::all_tuples,
};

mod sealed {
    use super::*;
    pub trait Sealed {}

    impl Sealed for () {}
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

pub trait RelationSet: Sized + Sealed {
    type Edges: ReadOnlyWorldQuery;
    type Types: 'static;
}

impl RelationSet for () {
    type Edges = ();
    type Types = ();
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

pub trait RelationEntries {
    fn hosts(&self, id: RelationId) -> &[Entity];
    fn targets(&self, id: RelationId) -> &[Entity];
}

impl<R: Relation> RelationEntries for RelationsItem<'_, R> {
    fn hosts(&self, id: RelationId) -> &[Entity] {
        if TypeId::of::<R>() == id.0 {
            self.edges.hosts()
        } else {
            &[]
        }
    }

    fn targets(&self, id: RelationId) -> &[Entity] {
        if TypeId::of::<R>() == id.0 {
            self.edges.targets()
        } else {
            &[]
        }
    }
}

impl<R: Relation> RelationEntries for RelationsItem<'_, Option<R>> {
    fn hosts(&self, id: RelationId) -> &[Entity] {
        if TypeId::of::<R>() == id.0 {
            self.edges.hosts()
        } else {
            &[]
        }
    }

    fn targets(&self, id: RelationId) -> &[Entity] {
        if TypeId::of::<R>() == id.0 {
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
            $(for<'a> <<$P::Edges as WorldQuery>::ReadOnly as WorldQuery>::Item<'a>: EdgeInfo,)*
        {
            fn hosts(&self, id: RelationId) -> &[Entity] {
                let ($($e,)*) = &self.edges;
                $(if TypeId::of::<$P::Types>() == id.0 { return  $e.hosts() })*
                &[]
            }

            fn targets(&self, id: RelationId) -> &[Entity] {
                let ($($e,)*) = &self.edges;
                $(if TypeId::of::<$P::Types>() == id.0 { return  $e.targets() })*
                &[]
            }
        }
    }
}

all_tuples!(impl_relation_entries, 1, 15, P, e);

/*pub trait Product<const N: usize, R: RelationSet> {
    fn product<'a>(edges: &RelationsItem<'a, R>) -> EdgeProduct<'a, N>;
}

macro_rules! impl_product {
    ($($P:ident),*) => {
        impl<$($P: EdgeQuery),*> Product<{ count!($($P )*) }> for ($($P,)*) {
            fn product<'a>(edges: &EdgeWQItem<'a>) -> EdgeProduct<'a, { count!($($P )*) }> {
                let base_iterators = [$(<$P as crate::operations::EdgeQuery>::entities(&edges),)*];
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

all_tuples!(impl_product, 1, 15, P);*/

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

pub trait HereditaryWorldQuery {}

pub trait Trackable: Sealed {}
