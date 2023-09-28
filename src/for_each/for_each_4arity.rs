use crate::tuple_traits::*;

use bevy::ecs::{
    entity::Entity,
    query::{ReadOnlyWorldQuery, WorldQuery},
    system::Query,
};

use std::{borrow::Borrow, collections::VecDeque};

use crate::{
    for_each::ControlFlow,
    operations::utils::{EdgeSide, Operations, Relations, RelationsItem},
};

pub trait ForEachPermutations3Arity<const N: usize> {
    type P0<'p0>;
    type P1<'p1>;
    type P2<'p2>;
    type P3<'p3>;

    fn for_each<Func, Ret>(self, func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'p0, 'p1, 'p2, 'p3> FnMut(
            &'f mut Self::P0<'p0>,
            &'f mut Self::P1<'p1>,
            &'f mut Self::P2<'p2>,
            Self::P3<'p3>,
        ) -> Ret;
}
