use crate::{
    relation::{EdgeWQ, Relation},
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

pub struct EdgeProduct<'a, const N: usize> {
    pub(crate) base_iterators: [EdgeIter<'a>; N],
    pub(crate) live_iterators: [EdgeIter<'a>; N],
    pub(crate) entities: [Option<Entity>; N],
}

impl<'a, const N: usize> EdgeProduct<'a, N> {
    fn advance(&mut self, matches: [bool; N]) -> Option<[Entity; N]> {
        let n = matches
            .iter()
            .enumerate()
            .find_map(|(n, matches)| (!matches).then_some(n))
            .unwrap_or(N);

        for i in (n..N).skip(1) {
            self.live_iterators[i] = self.base_iterators[i].clone();
            self.entities[i] = self.live_iterators[i].next();
        }

        'l: {
            for i in (0..n).skip(1).rev() {
                if let Some(entity) = self.live_iterators[i].next() {
                    self.entities[i] = Some(entity);
                    break 'l;
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

#[derive(WorldQuery)]
pub struct Relations<R: RelationSet> {
    pub(crate) edges: EdgeWQ,
    _filters: R::Filters,
    _phantom: PhantomData<R>,
}

pub struct Ops<Left, Joins = (), Right = (), Traversal = ()> {
    left: Left,
    joins: PhantomData<Joins>,
    right: Right,
    traversal: Traversal,
}

pub struct BreadthFirstTraversal<T, E, I>
where
    T: Relation,
    E: Borrow<Entity>,
    I: IntoIterator<Item = E>,
{
    roots: I,
    _phantom: PhantomData<(T, E)>,
}

trait AeryQueryExt {
    fn ops(&self) -> Ops<&Self>;
    fn ops_mut(&mut self) -> Ops<&mut Self>;
}

impl<'w, 's, Q, F, R> AeryQueryExt for Query<'w, 's, (Q, Relations<R>), F>
where
    Q: WorldQuery,
    F: ReadOnlyWorldQuery,
    R: RelationSet + Send + Sync,
{
    fn ops(&self) -> Ops<&Self> {
        Ops {
            left: self,
            joins: PhantomData,
            right: (),
            traversal: (),
        }
    }

    fn ops_mut(&mut self) -> Ops<&mut Self> {
        Ops {
            left: self,
            joins: PhantomData,
            right: (),
            traversal: (),
        }
    }
}

pub trait BreadthFirst<E, I>
where
    E: Borrow<Entity>,
    I: IntoIterator<Item = E>,
{
    type Out<T: Relation>;
    fn breadth_first<T: Relation>(self, roots: I) -> Self::Out<T>;
}

impl<Left, Joins, Right, Traversal, E, I> BreadthFirst<E, I> for Ops<Left, Joins, Right, Traversal>
where
    E: Borrow<Entity>,
    I: IntoIterator<Item = E>,
{
    type Out<T: Relation> = Ops<Left, Joins, Right, BreadthFirstTraversal<T, E, I>>;
    fn breadth_first<T: Relation>(self, roots: I) -> Self::Out<T> {
        Ops {
            left: self.left,
            joins: self.joins,
            right: self.right,
            traversal: BreadthFirstTraversal {
                roots,
                _phantom: PhantomData,
            },
        }
    }
}

pub trait Join<Item>
where
    Item: for<'a> Joinable<'a, 1>,
{
    type Out<T: Relation>;
    fn join<T: Relation>(self, item: Item) -> Self::Out<T>;
}

impl<Item, Left, Joins, Right, Traversal> Join<Item> for Ops<Left, Joins, Right, Traversal>
where
    Item: for<'a> Joinable<'a, 1>,
    Joins: Append,
    Right: Append,
{
    type Out<T: Relation> =
        Ops<Left, <Joins as Append>::Out<T>, <Right as Append>::Out<Item>, Traversal>;

    fn join<T: Relation>(self, item: Item) -> Self::Out<T> {
        Ops {
            left: self.left,
            joins: PhantomData,
            right: Append::append(self.right, item),
            traversal: self.traversal,
        }
    }
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

pub trait TrappedForEach {
    type In<'i>;
    fn for_each<Func, Ret>(self, func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: FnMut(Self::In<'_>) -> Ret;
}

impl<Q, R, F, T, E, I> TrappedForEach
    for Ops<&'_ Query<'_, '_, (Q, Relations<R>), F>, (), (), BreadthFirstTraversal<T, E, I>>
where
    Q: WorldQuery,
    R: RelationSet,
    F: ReadOnlyWorldQuery,
    T: Relation,
    E: Borrow<Entity>,
    I: IntoIterator<Item = E>,
{
    type In<'i> = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'i>;
    fn for_each<Func, Ret>(self, mut func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: FnMut(Self::In<'_>) -> Ret,
    {
        let mut queue = self
            .traversal
            .roots
            .into_iter()
            .map(|e| *e.borrow())
            .collect::<VecDeque<Entity>>();

        while let Some((left, relations)) = queue.pop_front().and_then(|e| self.left.get(e).ok()) {
            for e in relations.edges.edges.iter_fosters::<T>() {
                queue.push_back(e);
            }

            if let ControlFlow::Exit = func(left).into() {
                return;
            }
        }
    }
}

impl<Q, R, F, T, E, I> TrappedForEach
    for Ops<&'_ mut Query<'_, '_, (Q, Relations<R>), F>, (), (), BreadthFirstTraversal<T, E, I>>
where
    Q: WorldQuery,
    R: RelationSet,
    F: ReadOnlyWorldQuery,
    T: Relation,
    E: Borrow<Entity>,
    I: IntoIterator<Item = E>,
{
    type In<'i> = <Q as WorldQuery>::Item<'i>;
    fn for_each<Func, Ret>(self, mut func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: FnMut(Self::In<'_>) -> Ret,
    {
        let mut queue = self
            .traversal
            .roots
            .into_iter()
            .map(|e| *e.borrow())
            .collect::<VecDeque<Entity>>();

        while let Some((left, relations)) =
            queue.pop_front().and_then(|e| self.left.get_mut(e).ok())
        {
            for e in relations.edges.edges.iter_fosters::<T>() {
                queue.push_back(e);
            }

            if let ControlFlow::Exit = func(left).into() {
                return;
            }
        }
    }
}

pub trait InnerForEach<const N: usize> {
    type Left<'l>;
    type Right<'r>;
    fn for_each<Func, Ret>(self, func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'l, 'r> FnMut(&'f mut Self::Left<'l>, Self::Right<'r>) -> Ret;
}

impl<Q, R, F, Joins, Right, const N: usize> InnerForEach<N>
    for Ops<&'_ Query<'_, '_, (Q, Relations<R>), F>, Joins, Right, ()>
where
    Q: WorldQuery,
    R: RelationSet,
    F: ReadOnlyWorldQuery,
    Joins: Product<N>,
    Right: for<'a> Joinable<'a, N>,
{
    type Left<'l> = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'l>;
    type Right<'r> = <Right as Joinable<'r, N>>::Out;
    fn for_each<Func, Ret>(mut self, mut func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'l, 'r> FnMut(&'f mut Self::Left<'l>, Self::Right<'r>) -> Ret,
    {
        for (mut left, relations) in self.left.iter() {
            let mut edge_product = Joins::product(relations.edges);
            let mut matches = [true; N];
            while let Some(entities) = edge_product.advance(matches) {
                matches = Joinable::check(&self.right, entities);

                if matches.iter().any(|b| !b) {
                    continue;
                }

                if let ControlFlow::Exit =
                    func(&mut left, Joinable::join(&mut self.right, entities)).into()
                {
                    return;
                }
            }
        }
    }
}

impl<Q, R, F, Joins, Right, const N: usize> InnerForEach<N>
    for Ops<&'_ mut Query<'_, '_, (Q, Relations<R>), F>, Joins, Right, ()>
where
    Q: WorldQuery,
    R: RelationSet,
    F: ReadOnlyWorldQuery,
    Joins: Product<N>,
    Right: for<'a> Joinable<'a, N>,
{
    type Left<'l> = <Q as WorldQuery>::Item<'l>;
    type Right<'r> = <Right as Joinable<'r, N>>::Out;
    fn for_each<Func, Ret>(mut self, mut func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'l, 'r> FnMut(&'f mut Self::Left<'l>, Self::Right<'r>) -> Ret,
    {
        for (mut left, relations) in self.left.iter_mut() {
            let mut edge_product = Joins::product(relations.edges);
            let mut matches = [true; N];
            while let Some(entities) = edge_product.advance(matches) {
                matches = Joinable::check(&self.right, entities);

                if matches.iter().any(|b| !b) {
                    continue;
                }

                if let ControlFlow::Exit =
                    func(&mut left, Joinable::join(&mut self.right, entities)).into()
                {
                    return;
                }
            }
        }
    }
}

impl<Q, R, F, T, E, I, Joins, Right, const N: usize> InnerForEach<N>
    for Ops<&'_ Query<'_, '_, (Q, Relations<R>), F>, Joins, Right, BreadthFirstTraversal<T, E, I>>
where
    Q: WorldQuery,
    R: RelationSet,
    F: ReadOnlyWorldQuery,
    T: Relation,
    E: Borrow<Entity>,
    I: IntoIterator<Item = E>,
    Joins: Product<N>,
    Right: for<'a> Joinable<'a, N>,
{
    type Left<'l> = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'l>;
    type Right<'r> = <Right as Joinable<'r, N>>::Out;
    fn for_each<Func, Ret>(mut self, mut func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'l, 'r> FnMut(&'f mut Self::Left<'l>, Self::Right<'r>) -> Ret,
    {
        let mut queue = self
            .traversal
            .roots
            .into_iter()
            .map(|e| *e.borrow())
            .collect::<VecDeque<Entity>>();

        while let Some((mut left, relations)) =
            queue.pop_front().and_then(|e| self.left.get(e).ok())
        {
            for e in relations.edges.edges.iter_fosters::<T>() {
                queue.push_back(e);
            }

            let mut edge_product = Joins::product(relations.edges);
            let mut matches = [true; N];
            while let Some(entities) = edge_product.advance(matches) {
                matches = Joinable::check(&self.right, entities);

                if matches.iter().any(|b| !b) {
                    continue;
                }

                if let ControlFlow::Exit =
                    func(&mut left, Joinable::join(&mut self.right, entities)).into()
                {
                    return;
                }
            }
        }
    }
}

impl<Q, R, F, T, E, I, Joins, Right, const N: usize> InnerForEach<N>
    for Ops<
        &'_ mut Query<'_, '_, (Q, Relations<R>), F>,
        Joins,
        Right,
        BreadthFirstTraversal<T, E, I>,
    >
where
    Q: WorldQuery,
    R: RelationSet,
    F: ReadOnlyWorldQuery,
    T: Relation,
    E: Borrow<Entity>,
    I: IntoIterator<Item = E>,
    Joins: Product<N>,
    Right: for<'a> Joinable<'a, N>,
{
    type Left<'l> = <Q as WorldQuery>::Item<'l>;
    type Right<'r> = <Right as Joinable<'r, N>>::Out;
    fn for_each<Func, Ret>(mut self, mut func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'l, 'r> FnMut(&'f mut Self::Left<'l>, Self::Right<'r>) -> Ret,
    {
        let mut queue = self
            .traversal
            .roots
            .into_iter()
            .map(|e| *e.borrow())
            .collect::<VecDeque<Entity>>();

        while let Some((mut left, relations)) =
            queue.pop_front().and_then(|e| self.left.get_mut(e).ok())
        {
            for e in relations.edges.edges.iter_fosters::<T>() {
                queue.push_back(e);
            }

            let mut edge_product = Joins::product(relations.edges);
            let mut matches = [true; N];
            while let Some(entities) = edge_product.advance(matches) {
                matches = Joinable::check(&self.right, entities);

                if matches.iter().any(|b| !b) {
                    continue;
                }

                if let ControlFlow::Exit =
                    func(&mut left, Joinable::join(&mut self.right, entities)).into()
                {
                    return;
                }
            }
        }
    }
}

#[cfg(test)]
#[allow(dead_code)]
#[allow(unused_variables)]
mod compile_tests {
    use super::*;
    use bevy::prelude::*;

    #[derive(Component)]
    struct A;

    #[derive(Component)]
    struct B;

    #[derive(Component)]
    struct C;

    struct R0;

    impl Relation for R0 {}

    struct R1;

    impl Relation for R1 {}

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

    fn breadth_first_immut(left: Query<(&A, Relations<(R0, R1)>)>) {
        left.ops()
            .breadth_first::<R0>(None::<Entity>)
            .for_each(|a| {});
    }

    fn breadth_first_immut_joined(left: Query<(&A, Relations<(R0, R1)>)>, right: Query<&B>) {
        left.ops()
            .breadth_first::<R0>(None::<Entity>)
            .join::<R1>(&right)
            .for_each(|a, b| {});
    }

    fn breadth_first_mut(mut left: Query<(&mut A, Relations<(R0, R1)>)>) {
        left.ops_mut()
            .breadth_first::<R0>(None::<Entity>)
            .for_each(|a| {});
    }

    fn breadth_first_mut_joined_mut(
        left: Query<(&A, Relations<(R0, R1)>)>,
        mut right: Query<&mut B>,
    ) {
        left.ops()
            .breadth_first::<R0>(None::<Entity>)
            .join::<R1>(&mut right)
            .for_each(|a, b| {});
    }
}
