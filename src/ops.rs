use crate::{relation::Relation, Edges, RelationSet};
use bevy::ecs::{
    entity::Entity,
    query::{ReadOnlyWorldQuery, WorldQuery},
    system::Query,
};
use std::{
    borrow::Borrow,
    collections::{HashSet, VecDeque},
    marker::PhantomData,
};

mod sealed {
    use super::*;
    pub trait Joinable {}

    impl<Q, F> Joinable for &'_ Query<'_, '_, Q, F>
    where
        Q: WorldQuery,
        F: ReadOnlyWorldQuery,
    {
    }

    impl<Q, F> Joinable for &'_ mut Query<'_, '_, Q, F>
    where
        Q: WorldQuery,
        F: ReadOnlyWorldQuery,
    {
    }

    pub trait RelationQuery {}
}

use sealed::*;

#[derive(WorldQuery)]
pub struct Relations<R: RelationSet> {
    edges: &'static Edges,
    _filters: R::Filters,
    _phantom: PhantomData<R>,
}

pub struct Ops<Left, R, C, Joins = (), Right = ()>
where
    R: Relation,
    Left: IntoIterator<Item = (Relations<R>, C)>,
{
    left: Left,
    joins: Joins,
    right: Right,
}

pub trait Join<Item: Joinable> {
    type Out;
    fn join<T: Relation>(item: Item) -> Self::Out;
}

pub trait BreadthFirst<E: Borrow<Entity>> {
    type Out;
    type OutMut;
    fn breadth_first<T: Relation>(&self, roots: impl IntoIterator<Item = E>) -> Self::Out;
    fn breadth_frist_mut<T: Relation>(&self, roots: impl IntoIterator<Item = E>) -> Self::OutMut;
}

pub enum ControlFlow<T> {
    Return(T),
    Continue,
}

impl From<()> for ControlFlow<()> {
    fn from(_: ()) -> Self {
        ControlFlow::Continue
    }
}

pub trait ForEachInner {
    type Components<'c>;
    type Joins<'j>;
    fn for_each_inner<Func, Cf, Ret>(self, func: Func)
    where
        Cf: Into<ControlFlow<Ret>>,
        Func: for<'f, 'c, 'j> FnMut(&'f mut Self::Components<'c>, Self::Joins<'j>) -> Cf;
}
