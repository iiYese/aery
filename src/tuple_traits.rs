use crate::{
    edges::{Edges, Hosts, Targets},
    //operations::EdgeProduct,
    relation::Relation,
};

use seq_macro::seq;

use bevy::{
    ecs::{
        entity::Entity,
        query::{AnyOf, ReadOnlyWorldQuery, WorldQuery},
        system::Query,
    },
    utils::all_tuples,
};

pub trait PadMax {
    type Padded;
}

#[rustfmt::skip]
impl<R0: Relation> PadMax
for R0
{
    type Padded = (R0, (), (), (), (), (), (), (), (), (), (), (), (), (), (), ());
}

#[rustfmt::skip]
impl<P0> PadMax
for (P0,)
{
    type Padded = (P0, (), (), (), (), (), (), (), (), (), (), (), (), (), (), ());
}

#[rustfmt::skip]
impl<P0, P1> PadMax
for (P0, P1)
{
    type Padded = (P0, P1, (), (), (), (), (), (), (), (), (), (), (), (), (), ());
}

#[rustfmt::skip]
impl<P0, P1, P2> PadMax
for (P0, P1, P2)
{
    type Padded = (P0, P1, P2, (), (), (), (), (), (), (), (), (), (), (), (), ());
}

#[rustfmt::skip]
impl<P0, P1, P2, P3> PadMax
for (P0, P1, P2, P3)
{
    type Padded = (P0, P1, P2, P3, (), (), (), (), (), (), (), (), (), (), (), ());
}

#[rustfmt::skip]
impl<P0, P1, P2, P3, P4> PadMax
for (P0, P1, P2, P3, P4)
{
    type Padded = (P0, P1, P2, P3, P4, (), (), (), (), (), (), (), (), (), (), ());
}

#[rustfmt::skip]
impl<P0, P1, P2, P3, P4, P5> PadMax
for (P0, P1, P2, P3, P4, P5)
{
    type Padded = (P0, P1, P2, P3, P4, P5, (), (), (), (), (), (), (), (), (), ());
}

#[rustfmt::skip]
impl<P0, P1, P2, P3, P4, P5, P6> PadMax
for (P0, P1, P2, P3, P4, P5, P6)
{
    type Padded = (P0, P1, P2, P3, P4, P5, P6, (), (), (), (), (), (), (), (), ());
}

#[rustfmt::skip]
impl<P0, P1, P2, P3, P4, P5, P6, P7> PadMax
for (P0, P1, P2, P3, P4, P5, P6, P7)
{
    type Padded = (P0, P1, P2, P3, P4, P5, P6, P7, (), (), (), (), (), (), (), ());
}

#[rustfmt::skip]
impl<P0, P1, P2, P3, P4, P5, P6, P7, P8> PadMax
for (P0, P1, P2, P3, P4, P5, P6, P7, P8)
{
    type Padded = (P0, P1, P2, P3, P4, P5, P6, P7, P8, (), (), (), (), (), (), ());
}

#[rustfmt::skip]
impl<P0, P1, P2, P3, P4, P5, P6, P7, P8, P9> PadMax
for (P0, P1, P2, P3, P4, P5, P6, P7, P8, P9)
{
    type Padded = (P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, (), (), (), (), (), ());
}

#[rustfmt::skip]
impl<P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10> PadMax
for (P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10)
{
    type Padded = (P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, (), (), (), (), ());
}

#[rustfmt::skip]
impl<P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11> PadMax
for (P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11)
{
    type Padded = (P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, (), (), (), ());
}

#[rustfmt::skip]
impl<P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12> PadMax
for (P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12)
{
    type Padded = (P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, (), (), ());
}

#[rustfmt::skip]
impl<P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13> PadMax
for (P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13)
{
    type Padded = (P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, (), ());
}

#[rustfmt::skip]
impl<P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14> PadMax
for (P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14)
{
    type Padded = (P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, ());
}

#[rustfmt::skip]
impl<P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15> PadMax
for (P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15)
{
    type Padded = (P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15);
}

mod sealed {
    use super::*;
    pub trait Sealed {}

    impl Sealed for () {}
    impl<R: Relation> Sealed for R {}
    impl<R: RelationSet> Sealed for Option<R> {}

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

trait TupleLens<Types, Key, const POS: usize> {
    type Out;
    fn get(&self) -> &Self::Out;
}

seq!(N in 0..16 {
    #[rustfmt::skip]
    impl<
        E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15,
        T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15,
    >
        TupleLens<(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15), T~N, N>
    for
        (E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15)
    {
        type Out = E~N;
        fn get(&self) -> &Self::Out {
            &self.N
        }
    }
});

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
    type Types;
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

/*pub trait Product<const N: usize> {
    fn product<'a>(edges: &EdgeWQItem<'a>) -> EdgeProduct<'a, N>;
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

pub trait Trackable: Sealed {}
