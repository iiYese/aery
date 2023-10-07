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
/// #[derive(Component)]
/// struct C;
///
/// #[derive(Component)]
/// struct D;
///
/// #[derive(Relation)]
/// struct R0;
///
/// #[derive(Relation)]
/// struct R1;
///
/// fn traversals(
///     mut abcs: Query<((&A, &mut B, &C), Relations<(R0, R1)>)>,
///     ds: Query<&D>,
///     starts: Query<Entity, Root<R0>>
/// ) {
///     // descent
///     abcs.traverse::<R0>(starts.iter()).for_each(|(a, b, c), _| {
///         // ..
///     });
///
///     // ascent
///     abcs.traverse::<Up<R0>>(starts.iter()).for_each(|(a, b, c), _| {
///         // ..
///     });
///
///     // mut access
///     abcs.traverse_mut::<R0>(starts.iter()).for_each(|(a, b, c), _| {
///         // ..
///     });
///
///     // with joins
///     abcs.traverse::<R0>(starts.iter()).for_each(|(a, b, c), edges| {
///         edges.join::<R1>(&ds).for_each(|d| {
///             // ..
///         });
///     });
/// }
///
///# //
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

/// Lets a traversing query track itself. This is just immediate ancestors/descendants depending
/// on if the traversal is a descent or ascent. See [`Track`] for an explaination on tracking.
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
///         .for_each(|(parent_a, parent_b), _, (child_a, child_b), _| {
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

/// Track the last seen from a query when traversing an edge. This is useful in scenarios
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
/// struct Node {
///     // ..
/// }
///
/// #[derive(Component)]
/// struct ScrollArea {
///     // ..
/// }
///
/// #[derive(Relation)]
/// struct Ui;
///
/// fn draw_node_graph(
///     mut scroll_areas: Query<&mut ScrollArea>,
///     tree: Query<(Option<&Node>, Relations<Ui>)>,
///     roots: Query<Entity, Root<Ui>>
/// ) {
///     tree.traverse::<Ui>(roots.iter())
///         .track(&mut scroll_areas)
///         .for_each(|scroll_area, node, _| {
///             if let Some(node) = node {
///                 // draw node to scroll area
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
/// - The init closure recieves the components of the entity who's breadth we're currently passing
/// & returns an inital value.
/// - The fold closure recieves the componetns from each entity in the breadth, the accumulated
/// value so far & returns a result.
/// - Returning an [`Ok`] from the fold closure advances it to the next item in the breadth.
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
///                 /* get available space for on child layout direction */
///                 # 0
///             },
///             |available, (size, _, spacing), _| {
///                 /* calculate size of children that take up a proportion of the free space */
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
/// # Examples
/// ### Simple join
/// ```
/// use bevy::prelude::*;
/// use aery::prelude::*;
///
/// #[derive(Component)]
/// struct Pos(Vec2);
///
/// // Instancing geometry from vector graphics
/// #[derive(Component)]
/// struct DynamicMesh {
///     // ..
/// }
///
/// #[derive(Relation)]
/// #[aery(Total)]
/// struct InstanceOf;
///
/// fn render_all_instances(
///     meshes: Query<(&DynamicMesh, Relations<InstanceOf>)>,
///     instances: Query<&Pos>,
/// ) {
///     for (mesh, edges) in meshes.iter() {
///         edges.join::<InstanceOf>(&instances).for_each(|pos| {
///             // Render mesh
///         })
///     }
/// }
///
/// # fn in_camera_view() -> bool { true }
/// // or join the other way aroind
/// fn render_in_view(
///     camera: Query<&Camera>,
///     meshes: Query<&DynamicMesh>,
///     instances: Query<(&Pos, Relations<InstanceOf>)>,
///
/// ) {
///     for (pos, edges) in instances.iter() {
///         // The API is still `for_each` but there will only be at most 1 match in this case
///         // because edges are exclusive by default
///         edges.join::<Up<InstanceOf>>(&meshes).for_each(|mesh| {
///             if !in_camera_view() { return }
///             // Draw mesh
///         });
///     }
/// }
/// ```
/// ### Multiple joins
/// ```
/// use bevy::prelude::*;
/// use aery::prelude::*;
///
/// #[derive(Component)]
/// struct A(u32);
///
/// #[derive(Component)]
/// struct B(u32);
///
/// #[derive(Component)]
/// struct C(u32);
///
/// #[derive(Relation)]
/// struct R0;
///
/// #[derive(Relation)]
/// struct R1;
///
/// fn setup(world: &mut World) {
///     let [a0, a1, a2] = std::array::from_fn(|n| world.spawn(A(n as u32)).id());
///
///     world.spawn(B(0))
///          .set::<R0>(a0);
///
///     world.spawn_empty()
///          .set::<R0>(a1);
///
///     world.spawn(B(1))
///          .set::<R0>(a2)
///          .set::<R1>(a1);
///
///     world.spawn(C(0))
///          .set::<R1>(a0)
///          .set::<R1>(a1);
///
///     world.spawn(C(1))
///          .set::<R1>(a0);
///
///     world.spawn(C(2));
///
///     world.spawn(C(3))
///          .set::<R1>(a2);
/// }
///
/// fn sys(a: Query<(&A, Relations<(R0, R1)>)>, b: Query<&B>, c: Query<&C>) {
///     for (a, edges) in a.iter() {
///         // Iterates through permutations of matches
///         edges.join::<R0>(&b).join::<R1>(&c).for_each(|(b, c)| {
///             println!("A({}), B({}), C({})", a.0, b.0, c.0)
///             // Prints:
///             // A(0), B(0), C(0)
///             // A(0), B(0), C(1)
///             // A(1), B(1), C(3)
///         });
///     }
/// }
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
        let _ = RS::ZST_OR_PANIC;
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
