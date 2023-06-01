use crate::{ops::EdgeProduct, Edges, Participates, Relation};
use bevy::{
    ecs::{entity::Entity, query::ReadOnlyWorldQuery},
    utils::all_tuples,
};
use std::marker::PhantomData;

macro_rules! replace {
    ($_:tt $sub:tt) => {
        $sub
    };
}

macro_rules! count {
    () => { 0 };
    ($_:tt $($tail:tt)*) => { 1  + count!($($tail)*) };
}

mod sealed {
    use super::*;
    pub trait RelationSet {
        type Filters: ReadOnlyWorldQuery;
        const SIZE: usize;
    }

    impl<R: Relation> RelationSet for R {
        type Filters = Participates<R>;
        const SIZE: usize = 1;
    }

    macro_rules! impl_relation_set {
        ($($p:ident),*) => {
            impl<$($p: Relation),*> RelationSet for ($($p,)*) {
                type Filters = ($(Participates<$p>,)*);
                const SIZE: usize = count!($($p )*);
            }
        };
    }

    all_tuples!(impl_relation_set, 1, 15, P);
}

pub use sealed::*;
