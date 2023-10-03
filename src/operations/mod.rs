use crate::{relation::ZstOrPanic, tuple_traits::*};

use bevy::ecs::{
    entity::Entity,
    query::{ReadOnlyWorldQuery, WorldQuery},
    system::Query,
};

use std::{borrow::Borrow, marker::PhantomData};

pub mod utils;
use utils::*;

pub trait Traverse<E, I>
where
    E: Borrow<Entity>,
    I: IntoIterator<Item = E>,
{
    fn traverse<Edge: EdgeSide>(&self, starts: I) -> TraverseAnd<&'_ Self, Edge, I>;
    fn traverse_mut<Edge: EdgeSide>(&mut self, starts: I) -> TraverseAnd<&'_ mut Self, Edge, I>;
}

impl<E, I, Q, RS, F> Traverse<E, I> for Query<'_, '_, (Q, Relations<RS>), F>
where
    E: Borrow<Entity>,
    I: IntoIterator<Item = E>,
    Q: WorldQuery,
    F: ReadOnlyWorldQuery,
    RS: RelationSet,
{
    fn traverse<Edge: EdgeSide>(&self, starts: I) -> TraverseAnd<&'_ Self, Edge, I> {
        let _ = RS::ZST_OR_PANIC;
        let _ = Edge::ZST_OR_PANIC;

        TraverseAnd {
            control: self,
            edge: PhantomData,
            starts,
            track: (),
            scan_init: (),
            scan_fold: (),
        }
    }

    fn traverse_mut<Edge: EdgeSide>(&mut self, starts: I) -> TraverseAnd<&'_ mut Self, Edge, I> {
        let _ = RS::ZST_OR_PANIC;
        let _ = Edge::ZST_OR_PANIC;

        TraverseAnd {
            control: self,
            edge: PhantomData,
            starts,
            track: (),
            scan_init: (),
            scan_fold: (),
        }
    }
}

pub trait TrackSelf {
    type Out;
    fn track_self(self) -> Self::Out;
}

impl<Control, Edge, Starts, Tracked> TrackSelf for TraverseAnd<Control, Edge, Starts, Tracked> {
    type Out = TraverseAnd<Control, Edge, Starts, Tracked, true>;

    fn track_self(self) -> Self::Out {
        TraverseAnd {
            control: self.control,
            edge: PhantomData,
            starts: self.starts,
            track: self.track,
            scan_init: (),
            scan_fold: (),
        }
    }
}

pub trait Track {
    type Out<Item>;
    fn track<Item>(self, item: Item) -> Self::Out<Item>
    where
        Item: for<'a> Trackable<'a, 1>;
}

impl<Control, Edge, Starts, Tracked, const ST: bool> Track
    for TraverseAnd<Control, Edge, Starts, Tracked, ST>
where
    Tracked: Append,
{
    type Out<Item> = TraverseAnd<Control, Edge, Starts, <Tracked as Append>::Out<Item>, ST>;

    fn track<Item>(self, item: Item) -> Self::Out<Item>
    where
        Item: for<'a> Trackable<'a, 1>,
    {
        TraverseAnd {
            control: self.control,
            edge: self.edge,
            starts: self.starts,
            track: Append::append(self.track, item),
            scan_init: (),
            scan_fold: (),
        }
    }
}

pub trait FoldBreadth<RS: RelationSet> {
    type WQ<'wq>;
    type Out<Init, Fold>;

    fn fold_breadth<Acc, E, Init, Fold>(self, init: Init, fold: Fold) -> Self::Out<Init, Fold>
    where
        Init: FnMut(&mut Self::WQ<'_>, &RelationsItem<RS>) -> Acc,
        Fold: FnMut(Acc, &mut Self::WQ<'_>, &RelationsItem<RS>) -> Result<Acc, E>;
}

impl<'a, 'w, 's, Q, RS, F, Edge, Starts> FoldBreadth<RS>
    for TraverseAnd<&'a Query<'w, 's, (Q, Relations<RS>), F>, Edge, Starts, (), true>
where
    Q: WorldQuery,
    F: ReadOnlyWorldQuery,
    RS: RelationSet,
{
    type WQ<'wq> = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'wq>;
    type Out<Init, Fold> =
        TraverseAnd<&'a Query<'w, 's, (Q, Relations<RS>), F>, Edge, Starts, (), true, Init, Fold>;

    fn fold_breadth<Acc, E, Init, Fold>(self, init: Init, fold: Fold) -> Self::Out<Init, Fold>
    where
        Init: FnMut(&mut Self::WQ<'_>, &RelationsItem<RS>) -> Acc,
        Fold: FnMut(Acc, &mut Self::WQ<'_>, &RelationsItem<RS>) -> Result<Acc, E>,
    {
        TraverseAnd {
            control: self.control,
            edge: PhantomData,
            starts: self.starts,
            track: self.track,
            scan_init: init,
            scan_fold: fold,
        }
    }
}

impl<'a, 'w, 's, Q, RS, F, Edge, Starts> FoldBreadth<RS>
    for TraverseAnd<&'a mut Query<'w, 's, (Q, Relations<RS>), F>, Edge, Starts, (), true>
where
    Q: WorldQuery,
    F: ReadOnlyWorldQuery,
    RS: RelationSet,
{
    type WQ<'wq> = <Q as WorldQuery>::Item<'wq>;
    type Out<Init, Fold> = TraverseAnd<
        &'a mut Query<'w, 's, (Q, Relations<RS>), F>,
        Edge,
        Starts,
        (),
        true,
        Init,
        Fold,
    >;

    fn fold_breadth<Acc, E, Init, Fold>(self, init: Init, fold: Fold) -> Self::Out<Init, Fold>
    where
        Init: FnMut(&mut Self::WQ<'_>, &RelationsItem<RS>) -> Acc,
        Fold: FnMut(Acc, &mut Self::WQ<'_>, &RelationsItem<RS>) -> Result<Acc, E>,
    {
        TraverseAnd {
            control: self.control,
            edge: PhantomData,
            starts: self.starts,
            track: self.track,
            scan_init: init,
            scan_fold: fold,
        }
    }
}

pub trait Join<Item>
where
    Item: for<'a> Joinable<'a, 1>,
{
    type Out<Edge: EdgeSide>;
    fn join<Edge: EdgeSide>(self, item: Item) -> Self::Out<Edge>;
}

impl<RS, Item> Join<Item> for &'_ RelationsItem<'_, RS>
where
    RS: RelationSet,
    Item: for<'a> Joinable<'a, 1>,
{
    type Out<Edge: EdgeSide> = JoinWith<Self, (Edge,), (Item,)>;

    fn join<Edge: EdgeSide>(self, item: Item) -> Self::Out<Edge> {
        let _ = Edge::ZST_OR_PANIC;

        JoinWith {
            relations: self,
            edges: PhantomData,
            items: (item,),
        }
    }
}

impl<RI, Edges, Items, Item> Join<Item> for JoinWith<RI, Edges, Items>
where
    Item: for<'a> Joinable<'a, 1>,
    Edges: Append,
    Items: Append,
{
    type Out<Edge: EdgeSide> =
        JoinWith<RI, <Edges as Append>::Out<Edge>, <Items as Append>::Out<Item>>;

    fn join<Edge: EdgeSide>(self, item: Item) -> Self::Out<Edge> {
        let _ = Edge::ZST_OR_PANIC;

        JoinWith {
            relations: self.relations,
            edges: PhantomData,
            items: Append::append(self.items, item),
        }
    }
}

// TODO: Compile tests for scan_breadth & track
#[cfg(test)]
#[allow(dead_code)]
#[allow(unused_variables)]
mod compile_tests {
    use crate::{self as aery, prelude::*};
    use bevy::prelude::*;

    #[derive(Component)]
    struct A;

    #[derive(Component)]
    struct B;

    #[derive(Component)]
    struct C;

    #[derive(Relation)]
    #[cleanup(policy = "Counted")]
    #[multi]
    struct R0;

    #[derive(Relation)]
    struct R1;

    fn join_immut(left: Query<(&A, Relations<(R0, R1)>)>, b: Query<&B>, c: Query<&C>) {
        for (a, edges) in left.iter() {
            edges.join::<R0>(&b).join::<R1>(&c).for_each(|(b, c)| {});
        }
    }

    fn join_left_mut(mut left: Query<(&mut A, Relations<(R0, R1)>)>, b: Query<&C>, c: Query<&C>) {
        for (a, edges) in left.iter_mut() {
            edges.join::<R0>(&b).join::<R1>(&c).for_each(|(b, c)| {});
        }
    }

    fn join_right_mut(
        left: Query<(&A, Relations<(R0, R1)>)>,
        mut b: Query<&mut B>,
        mut c: Query<&mut C>,
    ) {
        for (a, edges) in left.iter() {
            edges
                .join::<R0>(&mut b)
                .join::<R1>(&mut c)
                .for_each(|(b, c)| {});
        }
    }

    fn join_full_mut(
        mut left: Query<(&mut A, Relations<(R0, R1)>)>,
        mut b: Query<&mut B>,
        mut c: Query<&mut C>,
    ) {
        for (a, edges) in left.iter_mut() {
            edges
                .join::<R0>(&mut b)
                .join::<R1>(&mut c)
                .for_each(|(b, c)| {});
        }
    }

    fn traverse_immut(left: Query<(&A, Relations<(R0, R1)>)>) {
        left.traverse::<R0>(None::<Entity>)
            .track_self()
            .for_each(|p, pr, c, cr| {});
    }

    fn traverse_join(left: Query<(&A, Relations<(R0, R1)>)>, right: Query<&B>) {
        left.traverse::<R0>(None::<Entity>).for_each(|a, rels| {
            rels.join::<R1>(&right).for_each(|right| {});
        });
    }

    fn traverse_mut_join(left: Query<(&A, Relations<(R0, R1)>)>, mut right: Query<&mut B>) {
        left.traverse::<R0>(None::<Entity>).for_each(|a, rels| {
            rels.join::<R1>(&mut right).for_each(|right| {});
        });
    }

    fn traverse_join_imperative_mi(mut left: Query<(&A, Relations<(R0, R1)>)>, right: Query<&B>) {
        left.traverse_mut::<R0>(None::<Entity>).for_each(|a, rels| {
            rels.join::<R1>(&right).for_each(|right| {});
        });
    }

    fn track(left: Query<(&A, Relations<R0>)>, right: Query<(&C, Relations<R1>)>) {
        left.traverse::<R0>(None::<Entity>)
            .track(&right)
            .for_each(|c, a, _| {});
    }

    fn track_right_mut(left: Query<(&A, Relations<R0>)>, mut right: Query<&mut C>) {
        left.traverse::<R0>(None::<Entity>)
            .track(&mut right)
            .for_each(|c, a, _| {});
    }

    /*fn traverse_mut(mut left: Query<(&mut A, Relations<(R0, R1)>)>) {
        left.ops_mut()
            .traverse::<R0>(Entity::PLACEHOLDER)
            .track_self()
            .for_each(|a0, a1| {});
    }

    fn traverse_immut_joined(left: Query<(&A, Relations<(R0, R1)>)>, right: Query<&B>) {
        left.ops()
            .traverse::<R0>(Entity::PLACEHOLDER)
            .track_self()
            .join::<R1>(&right)
            .for_each(|a0, a1, b| {});
    }

    fn traverse_mut_joined_mut(left: Query<(&A, Relations<(R0, R1)>)>, mut right: Query<&mut B>) {
        left.ops()
            .traverse::<R0>(Entity::PLACEHOLDER)
            .join::<R1>(&mut right)
            .track_self()
            .for_each(|a0, a1, b| {});
    }

    fn query_optional(left: Query<(&A, Relations<(R0, Option<R1>)>)>, b: Query<&B>, c: Query<&C>) {
        left.ops()
            .join::<R0>(&b)
            .join::<R1>(&c)
            .for_each(|a, (b, c)| {});
    }*/
}

/*#[cfg(test)]
mod tests {
    use crate::{self as aery, prelude::*};
    use bevy::{app::AppExit, prelude::*};

    #[derive(Component)]
    struct S;

    #[derive(Component, Debug)]
    struct A(i32);

    #[derive(Component, Debug)]
    struct B(i32);

    #[derive(Component, Debug)]
    struct C(i32);

    #[derive(Relation)]
    #[multi]
    struct R0;

    #[derive(Relation)]
    #[multi]
    struct R1;

    #[derive(Relation)]
    #[multi]
    struct R2;

    #[derive(Resource)]
    struct EntityList {
        entities: [Entity; 9],
    }

    #[test]
    fn left_scarce_permutations_o() {
        fn init(world: &mut World) {
            let entities = [
                world.spawn(A(0)).id(),
                world.spawn(A(0)).id(),
                world.spawn(A(0)).id(),
                //
                world.spawn(B(0)).id(),
                world.spawn(B(0)).id(),
                world.spawn(B(0)).id(),
                //
                world.spawn(C(0)).id(),
                world.spawn(C(0)).id(),
                world.spawn(C(0)).id(),
            ];

            let [_, a1, _, b0, _, b2, _, c1, _] = entities;

            let left = world.spawn(S).id();

            world
                .entity_mut(left)
                .set::<R0>(a1)
                .set::<R1>(b0)
                .set::<R1>(b2)
                .set::<R2>(c1);

            world.insert_resource(EntityList { entities });
        }

        fn run(
            left: Query<(&S, Relations<(R0, R1, R2)>)>,
            mut a: Query<&mut A>,
            mut b: Query<&mut B>,
            mut c: Query<&mut C>,
        ) {
            left.ops()
                .join::<Up<R0>>(&mut a)
                .join::<Up<R1>>(&mut b)
                .join::<Up<R2>>(&mut c)
                .for_each(|_, (mut a, mut b, mut c)| {
                    a.0 += 1;
                    b.0 += 1;
                    c.0 += 1;
                });
        }

        fn test(
            mut exit: EventWriter<AppExit>,
            entity_list: Res<EntityList>,
            a: Query<&A>,
            b: Query<&B>,
            c: Query<&C>,
        ) {
            let [a0, a1, a2, b0, b1, b2, c0, c1, c2] = entity_list.entities;

            assert_eq!(0, a.get(a0).unwrap().0);
            assert_eq!(2, a.get(a1).unwrap().0);
            assert_eq!(0, a.get(a2).unwrap().0);

            assert_eq!(1, b.get(b0).unwrap().0);
            assert_eq!(0, b.get(b1).unwrap().0);
            assert_eq!(1, b.get(b2).unwrap().0);

            assert_eq!(0, c.get(c0).unwrap().0);
            assert_eq!(2, c.get(c1).unwrap().0);
            assert_eq!(0, c.get(c2).unwrap().0);

            exit.send(AppExit);
        }

        App::new()
            .add_plugins(Aery)
            .add_systems(Update, (init, run, test).chain())
            .run();
    }

    #[test]
    fn left_scarce_permutations_x() {
        fn init(world: &mut World) {
            let entities = [
                world.spawn(A(0)).id(),
                world.spawn(A(0)).id(),
                world.spawn(A(0)).id(),
                //
                world.spawn(B(0)).id(),
                world.spawn(B(0)).id(),
                world.spawn(B(0)).id(),
                //
                world.spawn(C(0)).id(),
                world.spawn(C(0)).id(),
                world.spawn(C(0)).id(),
            ];

            let [a0, _, a2, _, b1, _, c0, _, c2] = entities;

            let left = world.spawn(S).id();

            world
                .entity_mut(left)
                .set::<R0>(a0)
                .set::<R0>(a2)
                .set::<R1>(b1)
                .set::<R2>(c0)
                .set::<R2>(c2);

            world.insert_resource(EntityList { entities });
        }

        fn run(
            left: Query<(&S, Relations<(R0, R1, R2)>)>,
            mut a: Query<&mut A>,
            mut b: Query<&mut B>,
            mut c: Query<&mut C>,
        ) {
            left.ops()
                .join::<Up<R0>>(&mut a)
                .join::<Up<R1>>(&mut b)
                .join::<Up<R2>>(&mut c)
                .for_each(|_, (mut a, mut b, mut c)| {
                    a.0 += 1;
                    b.0 += 1;
                    c.0 += 1;
                });
        }

        fn test(
            mut exit: EventWriter<AppExit>,
            entity_list: Res<EntityList>,
            a: Query<&A>,
            b: Query<&B>,
            c: Query<&C>,
        ) {
            let [a0, a1, a2, b0, b1, b2, c0, c1, c2] = entity_list.entities;

            assert_eq!(2, a.get(a0).unwrap().0);
            assert_eq!(0, a.get(a1).unwrap().0);
            assert_eq!(2, a.get(a2).unwrap().0);

            assert_eq!(0, b.get(b0).unwrap().0);
            assert_eq!(4, b.get(b1).unwrap().0);
            assert_eq!(0, b.get(b2).unwrap().0);

            assert_eq!(2, c.get(c0).unwrap().0);
            assert_eq!(0, c.get(c1).unwrap().0);
            assert_eq!(2, c.get(c2).unwrap().0);

            exit.send(AppExit);
        }

        App::new()
            .add_plugins(Aery)
            .add_systems(Update, (init, run, test).chain())
            .run();
    }

    #[test]
    fn left_abundant_permutations_o() {
        fn init(world: &mut World) {
            let entities = [
                world.spawn_empty().id(),
                world.spawn(A(0)).id(),
                world.spawn_empty().id(),
                //
                world.spawn(B(0)).id(),
                world.spawn_empty().id(),
                world.spawn(B(0)).id(),
                //
                world.spawn_empty().id(),
                world.spawn(C(0)).id(),
                world.spawn_empty().id(),
            ];

            let [a0, a1, a2, b0, b1, b2, c0, c1, c2] = entities;

            let left = world.spawn(S).id();

            world
                .entity_mut(left)
                .set::<R0>(a0)
                .set::<R0>(a1)
                .set::<R0>(a2)
                .set::<R1>(b0)
                .set::<R1>(b1)
                .set::<R1>(b2)
                .set::<R2>(c0)
                .set::<R2>(c1)
                .set::<R2>(c2);

            world.insert_resource(EntityList { entities });
        }

        fn run(
            left: Query<(&S, Relations<(R0, R1, R2)>)>,
            mut a: Query<&mut A>,
            mut b: Query<&mut B>,
            mut c: Query<&mut C>,
        ) {
            left.ops()
                .join::<Up<R0>>(&mut a)
                .join::<Up<R1>>(&mut b)
                .join::<Up<R2>>(&mut c)
                .for_each(|_, (mut a, mut b, mut c)| {
                    a.0 += 1;
                    b.0 += 1;
                    c.0 += 1;
                });
        }

        fn test(
            mut exit: EventWriter<AppExit>,
            entity_list: Res<EntityList>,
            a: Query<&A>,
            b: Query<&B>,
            c: Query<&C>,
        ) {
            let [_, a1, _, b0, _, b2, _, c1, _] = entity_list.entities;

            assert_eq!(2, a.get(a1).unwrap().0);
            assert_eq!(1, b.get(b0).unwrap().0);
            assert_eq!(1, b.get(b2).unwrap().0);
            assert_eq!(2, c.get(c1).unwrap().0);

            exit.send(AppExit);
        }

        App::new()
            .add_plugins(Aery)
            .add_systems(Update, (init, run, test).chain())
            .run();
    }

    #[test]
    fn left_abundant_permutations_x() {
        fn init(world: &mut World) {
            let entities = [
                world.spawn(A(0)).id(),
                world.spawn_empty().id(),
                world.spawn(A(0)).id(),
                //
                world.spawn_empty().id(),
                world.spawn(B(0)).id(),
                world.spawn_empty().id(),
                //
                world.spawn(C(0)).id(),
                world.spawn_empty().id(),
                world.spawn(C(0)).id(),
            ];

            let [a0, a1, a2, b0, b1, b2, c0, c1, c2] = entities;

            let left = world.spawn(S).id();

            world
                .entity_mut(left)
                .set::<R0>(a0)
                .set::<R0>(a1)
                .set::<R0>(a2)
                .set::<R1>(b0)
                .set::<R1>(b1)
                .set::<R1>(b2)
                .set::<R2>(c0)
                .set::<R2>(c1)
                .set::<R2>(c2);

            world.insert_resource(EntityList { entities });
        }

        fn run(
            left: Query<(&S, Relations<(R0, R1, R2)>)>,
            mut a: Query<&mut A>,
            mut b: Query<&mut B>,
            mut c: Query<&mut C>,
        ) {
            left.ops()
                .join::<Up<R0>>(&mut a)
                .join::<Up<R1>>(&mut b)
                .join::<Up<R2>>(&mut c)
                .for_each(|_, (mut a, mut b, mut c)| {
                    a.0 += 1;
                    b.0 += 1;
                    c.0 += 1;
                });
        }

        fn test(
            mut exit: EventWriter<AppExit>,
            entity_list: Res<EntityList>,
            a: Query<&A>,
            b: Query<&B>,
            c: Query<&C>,
        ) {
            let [a0, _, a2, _, b1, _, c0, _, c2] = entity_list.entities;

            assert_eq!(2, a.get(a0).unwrap().0);
            assert_eq!(2, a.get(a2).unwrap().0);
            assert_eq!(4, b.get(b1).unwrap().0);
            assert_eq!(2, c.get(c0).unwrap().0);
            assert_eq!(2, c.get(c2).unwrap().0);

            exit.send(AppExit);
        }

        App::new()
            .add_plugins(Aery)
            .add_systems(Update, (init, run, test).chain())
            .run();
    }
}*/
