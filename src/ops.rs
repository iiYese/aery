use crate::{
    relation::{EdgeWQ, Relation},
    tuple_traits::*,
};
use bevy::ecs::{
    entity::Entity,
    query::{ReadOnlyWorldQuery, WorldQuery},
    system::Query,
};
use std::{borrow::Borrow, marker::PhantomData};

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

impl<'w, 's, Q, F, R> AeryQueryExt for Query<'w, 's, (Relations<R>, Q), F>
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

pub trait InnerForEach<const N: usize> {
    type Components<'c>;
    type Joins<'j>;
    fn for_each<Func, Ret>(self, func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'c, 'j> FnMut(&'f mut Self::Components<'c>, Self::Joins<'j>) -> Ret;
}

impl<R, Q, F, P, J, const N: usize> InnerForEach<N>
    for Ops<&'_ Query<'_, '_, (Relations<R>, Q), F>, P, J, ()>
where
    R: RelationSet,
    Q: WorldQuery,
    F: ReadOnlyWorldQuery,
    P: Product<N>,
    J: for<'a> Joinable<'a, N>,
{
    type Components<'c> = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'c>;
    type Joins<'j> = <J as Joinable<'j, N>>::Out;
    fn for_each<Func, Ret>(mut self, mut func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'c, 'j> FnMut(&'f mut Self::Components<'c>, Self::Joins<'j>) -> Ret,
    {
        for (relations, mut components) in self.left.iter() {
            let mut edge_product = P::product(relations.edges);
            let mut matches = [true; N];
            while let Some(entities) = edge_product.advance(matches) {
                matches = Joinable::check(&self.right, entities);
                if matches.iter().all(|b| *b) {
                    let joins = Joinable::join(&mut self.right, entities);
                    if let ControlFlow::Exit = func(&mut components, joins).into() {
                        return;
                    }
                }
            }
        }
    }
}

mod test {
    use super::*;
    use bevy::prelude::*;

    #[derive(Component)]
    struct A;

    #[derive(Component)]
    struct B;

    struct R;

    impl Relation for R {}

    fn test(left: Query<(Relations<R>, &A)>, right: Query<&B>) {
        left.ops().join::<R>(&right).for_each(|_, _| {});
    }
}
