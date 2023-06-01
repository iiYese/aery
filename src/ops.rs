use crate::{relation::Relation, tuple_traits::*, Edges, Participates};
use bevy::{
    ecs::{
        entity::Entity,
        query::{ReadOnlyWorldQuery, WorldQuery},
        system::Query,
    },
    utils::all_tuples,
};
use std::{
    borrow::Borrow,
    collections::{HashSet, VecDeque},
    marker::PhantomData,
};

mod sealed {
    use super::*;
    pub trait Joinable<'a> {
        type Out;
        fn join(&'a mut self, entity: Entity) -> Option<Self::Out>;
    }

    impl<'a, Q, F> Joinable<'a> for &'_ Query<'_, '_, Q, F>
    where
        Q: WorldQuery,
        F: ReadOnlyWorldQuery,
    {
        type Out = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'a>;
        fn join(&'a mut self, entity: Entity) -> Option<Self::Out> {
            (**self).get(entity).ok()
        }
    }

    impl<'a, Q, F> Joinable<'a> for &'_ mut Query<'_, '_, Q, F>
    where
        Q: WorldQuery,
        F: ReadOnlyWorldQuery,
    {
        type Out = <Q as WorldQuery>::Item<'a>;
        fn join(&'a mut self, entity: Entity) -> Option<Self::Out> {
            (**self).get_mut(entity).ok()
        }
    }
}

use sealed::*;

type EdgeIter<'a> = std::iter::Flatten<
    std::option::IntoIter<std::iter::Copied<indexmap::set::Iter<'a, bevy::prelude::Entity>>>,
>;

pub(crate) struct EdgeProduct<'a, const N: usize> {
    base_iterators: [EdgeIter<'a>; N],
    live_iterators: [EdgeIter<'a>; N],
    values: [Option<Entity>; N],
}

impl<'a, const N: usize> EdgeProduct<'a, N> {
    fn values(&self) -> Option<[Entity; N]> {
        self.values
            .iter()
            .all(Option::is_some)
            .then(|| self.values.map(Option::unwrap))
    }

    fn advance(&mut self, matches: [bool; N]) {}
}

#[derive(WorldQuery)]
pub struct Relations<R: RelationSet> {
    pub(crate) edges: &'static Edges,
    _filters: R::Filters,
    _phantom: PhantomData<R>,
}

pub struct Joins<R, Items> {
    items: Items,
    _phantom: PhantomData<R>,
}

pub struct Ops<Left, Joins = (), Right = (), Traversal = ()> {
    left: Left,
    joins: PhantomData<Joins>,
    right: Right,
    traversal: PhantomData<Traversal>,
}

pub trait BreadthFirst<E: Borrow<Entity>> {
    type Out;
    fn breadth_first<T: Relation>(self, roots: impl IntoIterator<Item = E>) -> Self::Out;
}

pub trait Join<Item>
where
    Item: for<'a> Joinable<'a>,
{
    type Out;
    fn join<T: Relation>(self, item: Item) -> Self::Out;
}

pub enum ControlFlow {
    Continue,
    Exit,
}

impl From<()> for ControlFlow {
    fn from(_: ()) -> Self {
        ControlFlow::Continue
    }
}

pub trait InnerForEach {
    type Components<'c>;
    type Joins<'j>;
    fn for_each<Func, Ret>(self, func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'c, 'j> FnMut(&'f mut Self::Components<'c>, Self::Joins<'j>) -> Ret;
}

impl<R, Q, F, R0, J0> InnerForEach for Ops<&'_ Query<'_, '_, (Relations<R>, Q), F>, R0, J0, ()>
where
    R: RelationSet,
    Q: WorldQuery,
    F: ReadOnlyWorldQuery,
    R0: Relation,
    for<'a> J0: Joinable<'a>,
{
    type Components<'c> = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'c>;
    type Joins<'j> = <J0 as Joinable<'j>>::Out;
    fn for_each<Func, Ret>(self, mut func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'c, 'j> FnMut(&'f mut Self::Components<'c>, Self::Joins<'j>) -> Ret,
    {
        let mut j0 = self.right;

        for (relations, mut components) in self.left.iter() {
            for e0 in relations.edges.iter_targets::<R0>() {
                let Some(i0) = j0.join(e0) else {
                    continue
                };

                if let ControlFlow::Exit = func(&mut components, i0).into() {
                    return;
                }
            }
        }
    }
}

macro_rules! count_exprs {
    () => { 0 };
    ($e:expr) => { 1 };
    ($e:expr, $($es:expr),+) => { 1 + count_exprs!($($es),*) };
}
