use crate::{relation::ZstOrPanic, tuple_traits::*};

use bevy::ecs::{
    entity::Entity,
    query::{ReadOnlyWorldQuery, WorldQuery},
    system::Query,
};

use std::{borrow::Borrow, marker::PhantomData};

///
pub mod utils;
use utils::*;

/// Traverse a query in breadth first order.
/// Query must be in the form `Query<(Q, Relations<RS>)>` to use this API.
///
/// # Examples
/// ```
/// use bevy::prelude::*;
/// use aery::prelude::*;
///
/// #[derive(Component)]
/// struct A;
///
/// #[derive(Component)]
/// struct B;
///
/// #[derive(Relation)]
/// struct R;
///
/// fn descent(left: Query<((&A, &B), Relations<R>)>, starts: Query<Entity, Root<R>>) {
///     left.traverse::<R>(starts.iter()).for_each(|(a, b), _| {
///         // ..
///     })
/// }
///
/// fn ascent(left: Query<((&A, &B), Relations<R>)>, starts: Query<Entity, Root<R>>) {
///     left.traverse::<Up<R>>(starts.iter()).for_each(|(a, b), _| {
///         // ..
///     })
/// }
/// ```
pub trait Traverse<E, I>
where
    E: Borrow<Entity>,
    I: IntoIterator<Item = E>,
{
    /// Traverse along an edge.
    fn traverse<Edge: EdgeSide>(&self, starts: I) -> TraverseAnd<&'_ Self, Edge, I>;
    /// Traverse along an edge with mut access.
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
            init: (),
            fold: (),
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
            init: (),
            fold: (),
        }
    }
}

/// Let a traversing query track itself. This allows you to see immediate ancestors for desents &
/// immediate descendants for ascents. See [`Track`] for an explaination on tracking.
///
/// # Example
/// ```
/// use bevy::prelude::*;
/// use aery::prelude::*;
///
/// #[derive(Component)]
/// struct A;
///
/// #[derive(Component)]
/// struct B;
///
/// #[derive(Relation)]
/// struct R;
///
/// fn descent_with_parent(
///     left: Query<((&A, &B), Relations<R>)>,
///     starts: Query<Entity, Root<R>>
/// ) {
///     left.traverse::<R>(starts.iter())
///         .track_self()
///         .for_each(|(p_a, p_b), _, (c_a, c_b), _| {
///             // ..
///         })
/// }
/// ```
pub trait TrackSelf {
    #[allow(missing_docs)]
    type Out;

    #[allow(missing_docs)]
    fn track_self(self) -> Self::Out;
}

impl<Control, Edge, Starts> TrackSelf for TraverseAnd<Control, Edge, Starts> {
    type Out = TraverseAnd<Control, Edge, Starts, SelfTracking>;

    fn track_self(self) -> Self::Out {
        TraverseAnd {
            control: self.control,
            edge: PhantomData,
            starts: self.starts,
            track: SelfTracking,
            init: (),
            fold: (),
        }
    }
}

/// Track the last seen set of components when traversing an edge. This is useful in scenarios
/// where you mightn't have a component on every entity in a hierarchy. For instance it might not
/// make sense for a component to be on every entity or even an immediate ancestor meaning spam
/// propogation isn't viable. Scroll areas in UI are one example of this.
///
/// # Example
/// ```
/// use bevy::prelude::*;
/// use aery::prelude::*;
///
/// #[derive(Component)]
/// struct Text(String);
///
/// #[derive(Component)]
/// struct ScrollArea {
///     // ..
/// }
///
/// #[derive(Relation)]
/// struct Ui;
///
/// fn draw(
///     mut scroll_areas: Query<&mut ScrollArea>,
///     tree: Query<(Option<&Text>, Relations<Ui>)>,
///     roots: Query<Entity, Root<Ui>>
/// ) {
///     tree.traverse::<Ui>(roots.iter())
///         .track(&mut scroll_areas)
///         .for_each(|scroll_area, text, _| {
///             if let Some(text) = text {
///                 // draw text to scroll area
///             }
///         });
/// }
/// ```
pub trait Track {
    #[allow(missing_docs)]
    type Out<Item>;

    #[allow(missing_docs)]
    fn track<Item>(self, item: Item) -> Self::Out<Item>
    where
        Item: for<'a> Trackable<'a, 1>;
}

impl<Control, Edge, Starts, Tracked> Track for TraverseAnd<Control, Edge, Starts, Tracked>
where
    Tracked: Append,
{
    type Out<Item> = TraverseAnd<Control, Edge, Starts, <Tracked as Append>::Out<Item>>;

    fn track<Item>(self, item: Item) -> Self::Out<Item>
    where
        Item: for<'a> Trackable<'a, 1>,
    {
        TraverseAnd {
            control: self.control,
            edge: self.edge,
            starts: self.starts,
            track: Append::append(self.track, item),
            init: (),
            fold: (),
        }
    }
}

/// Passes each breadth twice doing a fold operation the first time for traversals that
/// [`TrackSelf`].
/// - The init closure recieves the ancestor entity & returns an initial value.
/// - The fold closure recieves each descendant & the accumulated value so far.
/// - Returning an [`Ok`] from the fold closure advances it to the next child.
/// - Returning an [`Err`] from the fold closure ends the fold & starts the second pass in the
/// `.for_each` immediately.
///
/// The result of the fold operation will be available in the `.for_each` call as the first
/// parameter.
/// ```
/// use bevy::prelude::*;
/// use aery::prelude::*;
///
/// #[derive(Component)]
/// struct ChildDir {
///     // ..
/// }
///
/// #[derive(Component)]
/// struct Size {
///     // ..
/// }
///
/// #[derive(Component)]
/// struct Spacing {
///     // ..
/// }
///
/// #[derive(Relation)]
/// struct Ui;
///
/// fn tiling_layout_algo(
///     roots: Query<Entity, Root<Ui>>,
///     mut tree: Query<((&mut Size, &ChildDir, &Spacing), Relations<Ui>)>
/// ) {
///     tree.traverse_mut::<Ui>(roots.iter())
///         .track_self()
///         .fold_breadth(
///             |(size, dir, _), _| {
///                 /* get available space based on child dir */
///                 # 0
///             },
///             |available, (size, _, spacing), _| {
///                 /* calculate actual space for each child */
///                 # Ok::<_, ()>(0)
///             }
///         )
///         .for_each(|res, parent, _, child, _| {
///             let Ok(res) = res else { return };
///             // subdivide parent space between children
///         });
/// }
///
/// ```
pub trait FoldBreadth<RS: RelationSet> {
    #[allow(missing_docs)]
    type WQ<'wq>;

    #[allow(missing_docs)]
    type Out<Init, Fold>;

    #[allow(missing_docs)]
    fn fold_breadth<Acc, E, Init, Fold>(self, init: Init, fold: Fold) -> Self::Out<Init, Fold>
    where
        Init: FnMut(&mut Self::WQ<'_>, &RelationsItem<RS>) -> Acc,
        Fold: FnMut(Acc, &mut Self::WQ<'_>, &RelationsItem<RS>) -> Result<Acc, E>;
}

impl<'a, 'w, 's, Q, RS, F, Edge, Starts> FoldBreadth<RS>
    for TraverseAnd<&'a Query<'w, 's, (Q, Relations<RS>), F>, Edge, Starts, SelfTracking>
where
    Q: WorldQuery,
    F: ReadOnlyWorldQuery,
    RS: RelationSet,
{
    type WQ<'wq> = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'wq>;
    type Out<Init, Fold> = TraverseAnd<
        &'a Query<'w, 's, (Q, Relations<RS>), F>,
        Edge,
        Starts,
        SelfTracking,
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
            init,
            fold,
        }
    }
}

impl<'a, 'w, 's, Q, RS, F, Edge, Starts> FoldBreadth<RS>
    for TraverseAnd<&'a mut Query<'w, 's, (Q, Relations<RS>), F>, Edge, Starts, SelfTracking>
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
        SelfTracking,
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
            init,
            fold,
        }
    }
}

/// Join multiple queries together via edges.
/// # Example
/// ```
/// use bevy::prelude::*;
/// use aery::prelude::*;
///
/// // Instancing geometry from vector graphics
/// #[derive(Component)]
/// struct DynamicMesh {
///     // ..
/// }
///
/// #[derive(Relation)]
/// #[aery(Total)]
/// struct Instance;
///
/// fn render_instances(
///     meshes: Query<(&DynamicMesh, Relations<Instance>)>,
///     instances: Query<&Transform>,
/// ) {
///     for (mesh, edges) in meshes.iter() {
///         edges.join::<Instance>(&instances).for_each(|instance| {
///             // Render mesh
///         })
///     }
/// }
///
/// ```
pub trait Join<Item>
where
    Item: for<'a> Joinable<'a, 1>,
{
    #[allow(missing_docs)]
    type Out<Edge: EdgeSide>;

    #[allow(missing_docs)]
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

#[cfg(test)]
#[allow(dead_code)]
#[allow(unused_variables)]
mod compile_tests {
    use crate::prelude::*;
    use bevy::prelude::*;

    #[derive(Component)]
    struct A;

    #[derive(Component)]
    struct B;

    #[derive(Component)]
    struct C;

    #[derive(Relation)]
    #[aery(Counted, Poly, Symmetric)]
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
            .fold_breadth(|_, _| 0, |x, _, _| Ok::<_, ()>(x))
            .for_each(|x, p, pr, c, cr| {});
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

#[cfg(test)]
mod tests {
    use crate::prelude::*;
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
    #[aery(Poly)]
    struct R0;

    #[derive(Relation)]
    #[aery(Poly)]
    struct R1;

    #[derive(Relation)]
    #[aery(Poly)]
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
            for (_, edges) in left.iter() {
                edges
                    .join::<Up<R0>>(&mut a)
                    .join::<Up<R1>>(&mut b)
                    .join::<Up<R2>>(&mut c)
                    .for_each(|(mut a, mut b, mut c)| {
                        a.0 += 1;
                        b.0 += 1;
                        c.0 += 1;
                    });
            }
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
            for (_, edges) in left.iter() {
                edges
                    .join::<Up<R0>>(&mut a)
                    .join::<Up<R1>>(&mut b)
                    .join::<Up<R2>>(&mut c)
                    .for_each(|(mut a, mut b, mut c)| {
                        a.0 += 1;
                        b.0 += 1;
                        c.0 += 1;
                    });
            }
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
            for (_, edges) in left.iter() {
                edges
                    .join::<Up<R0>>(&mut a)
                    .join::<Up<R1>>(&mut b)
                    .join::<Up<R2>>(&mut c)
                    .for_each(|(mut a, mut b, mut c)| {
                        a.0 += 1;
                        b.0 += 1;
                        c.0 += 1;
                    });
            }
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
            for (_, edges) in left.iter() {
                edges
                    .join::<Up<R0>>(&mut a)
                    .join::<Up<R1>>(&mut b)
                    .join::<Up<R2>>(&mut c)
                    .for_each(|(mut a, mut b, mut c)| {
                        a.0 += 1;
                        b.0 += 1;
                        c.0 += 1;
                    });
            }
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
}
