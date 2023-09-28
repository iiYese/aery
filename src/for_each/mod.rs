use crate::operations::utils::{EdgeSide, Operations, Relations, RelationsItem};
use crate::tuple_traits::*;

use bevy::ecs::{
    entity::Entity,
    query::{ReadOnlyWorldQuery, WorldQuery},
    system::Query,
};

use std::{borrow::Borrow, collections::VecDeque};

mod for_each_2arity;
mod for_each_3arity;
mod for_each_4arity;

pub use for_each_2arity::ForEachPermutations2Arity;
pub use for_each_3arity::ForEachPermutations3Arity;
pub use for_each_4arity::*;

pub trait ForEachBreadthFirst {
    type P0<'p0>;

    fn for_each<Func, Ret>(self, func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'p0> FnMut(&'f mut Self::P0<'p0>) -> Ret;
}

// ----------------------
// Beadth first traversal
// ----------------------

impl<Q, RS, F, T> ForEachBreadthFirst
    for Operations<&'_ Query<'_, '_, (Q, Relations<RS>), F>, (), (), T, Entity>
where
    Q: WorldQuery,
    RS: RelationSet,
    F: ReadOnlyWorldQuery,
    T: EdgeSide,
    for<'i> RelationsItem<'i, RS>: RelationEntries,
{
    type P0<'p0> = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'p0>;

    fn for_each<Func, Ret>(self, mut func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'p0> FnMut(&'f mut Self::P0<'p0>) -> Ret,
    {
        let mut queue = VecDeque::from([self.start]);

        'queue: while let Some(entity) = queue.pop_front() {
            let Ok((mut control, relations)) = self.control.get(entity) else {
                continue;
            };

            match func(&mut control).into() {
                ControlFlow::Exit => return,
                ControlFlow::Conclude => {
                    continue 'queue;
                }
                ControlFlow::Probe => {
                    queue.clear();
                }
                _ => {}
            }

            queue.extend(T::entities(&relations));
        }
    }
}

impl<Q, RS, F, T> ForEachBreadthFirst
    for Operations<&'_ mut Query<'_, '_, (Q, Relations<RS>), F>, (), (), T, Entity>
where
    Q: WorldQuery,
    RS: RelationSet,
    F: ReadOnlyWorldQuery,
    T: EdgeSide,
    for<'i> RelationsItem<'i, RS>: RelationEntries,
{
    type P0<'p0> = <Q as WorldQuery>::Item<'p0>;

    fn for_each<Func, Ret>(self, mut func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'p0> FnMut(&'f mut Self::P0<'p0>) -> Ret,
    {
        let mut queue = VecDeque::from([self.start]);

        'queue: while let Some(entity) = queue.pop_front() {
            let Ok((mut control, relations)) = self.control.get_mut(entity) else {
                continue;
            };

            match func(&mut control).into() {
                ControlFlow::Exit => return,
                ControlFlow::Conclude => {
                    continue 'queue;
                }
                ControlFlow::Probe => {
                    queue.clear();
                }
                _ => {}
            }

            queue.extend(T::entities(&relations));
        }
    }
}

#[cfg_attr(doc, aquamarine::aquamarine)]
/// Control flow enum for [`ForEachPermutations`] and [`ForEachPermutations3Arity`]. The closures
/// accepted by both return `impl Into<ControlFlow>` with `()` being turned into
/// `ControlFlow::Continue` to save you from typing `return ControlFlow::Continue` in all your
/// functions.
///
/// ```
///# use bevy::prelude::*;
///# use aery::prelude::*;
///#
///# #[derive(Component)]
///# struct A {
///#     // ..
///# }
///#
///# #[derive(Component)]
///# struct B {
///#     // ..
///# }
///#
///# #[derive(Relation)]
///# struct R;
///#
///# fn predicate(a: &A, b: &B) -> bool {
///#     true
///# }
///#
/// fn sys(a: Query<(&A, Relations<R>)>, b: Query<&B>) {
///     a.ops().join::<R>(&b).for_each(|a, b| {
///         if predicate(a, b) {
///             return ControlFlow::Exit;
///         }
///
///         // Return types still need to be the same so explicitly providing this is nessecary
///         // when doing any controlflow manipulation.
///         ControlFlow::Continue
///     })
/// }
/// ```
///
///
/// ## FastForward Illustration:
/// ```
/// use bevy::prelude::*;
/// use aery::prelude::*;
///
/// #[derive(Component)]
/// struct A;
///
/// #[derive(Component)]
/// struct B(usize);
///
/// #[derive(Component)]
/// struct C(usize);
///
/// #[derive(Component)]
/// struct D(usize);
///
/// #[derive(Relation)]
/// struct R0;
///
/// #[derive(Relation)]
/// struct R1;
///
/// #[derive(Relation)]
/// struct R2;
///
/// fn setup(mut commands: Commands) {
///     let bs = std::array::from_fn::<_, 3, _>(|n| commands.spawn(B(n)).id());
///     let cs = std::array::from_fn::<_, 3, _>(|n| commands.spawn(C(n)).id());
///     let ds = std::array::from_fn::<_, 3, _>(|n| commands.spawn(D(n)).id());
///
///     let a = commands.spawn(A).id();
///
///     for id in bs {
///         commands.add(Set::<R0>::new(a, id));
///     }
///
///     for id in cs {
///         commands.add(Set::<R1>::new(a, id));
///     }
///
///     for id in ds {
///         commands.add(Set::<R2>::new(a, id));
///     }
/// }
///
/// fn ff_sys(
///     a: Query<(&A, Relations<(R0, R1, R2)>)>,
///     b: Query<&B>,
///     c: Query<&C>,
///     d: Query<&D>
/// ) {
///     a.ops()
///         .join::<R0>(&b)
///         .join::<R1>(&c)
///         .join::<R2>(&d)
///         .for_each(|a, (b, c, d)| {
///             if c.0 == 1 {
///                 ControlFlow::FastForward(1)
///             }
///             else {
///                 println!("({}, {}, {})", b.0, c.0, d.0);
///                 ControlFlow::Continue
///             }
///         });
/// }
/// ```
/// ### Output of ff_sys:
/// ```ignore
///     (0, 0, 0)
///     (0, 0, 1)
///     (0, 0, 2)
/// //  Skipped:
/// //  (0, 1, 0)
/// //  (0, 1, 1)
/// //  (0, 1, 2)
///     (0, 2, 0)
///     (0, 2, 1)
///     (0, 2, 2)
///     (1, 0, 0)
///     (1, 0, 1)
///     (1, 0, 2)
/// //  Skipped:
/// //  (1, 1, 0)
/// //  (1, 1, 1)
/// //  (1, 1, 2)
///     (1, 2, 0)
///     (1, 2, 1)
///     (1, 2, 2)
///     (2, 0, 0)
///     (2, 0, 1)
///     (2, 0, 2)
/// //  Skipped:
/// //  (2, 1, 0)
/// //  (2, 1, 1)
/// //  (2, 1, 2)
///     (2, 2, 0)
///     (2, 2, 1)
///     (2, 2, 2)
/// ```
/// ## Walk Illustration:
/// ```
/// use bevy::prelude::*;
/// use aery::prelude::*;
///
/// #[derive(Component)]
/// struct A(usize);
///
/// #[derive(Component)]
/// struct B(usize);
///
/// #[derive(Relation)]
/// struct R0;
///
/// #[derive(Relation)]
/// struct R1;
///
/// fn setup(mut commands: Commands) {
///     commands.add(|wrld: &mut World| {
///         wrld.spawn(A(0))
///             .scope::<R0>(|_, mut ent1| {
///                 ent1.insert(A(1));
///                 ent1.scope_target::<R1>(|_, mut ent| { ent.insert(B(0)); })
///                     .scope_target::<R1>(|_, mut ent| { ent.insert(B(1)); });
///             })
///             .scope::<R0>(|_, mut ent2| {
///                 ent2.insert(A(2));
///                 ent2.scope_target::<R1>(|_, mut ent| { ent.insert(B(3)); })
///                     .scope_target::<R1>(|_, mut ent| { ent.insert(B(4)); });
///             })
///             .scope::<R0>(|_, mut ent3| {
///                 ent3.insert(A(3));
///                 ent3.scope_target::<R1>(|_, mut ent| { ent.insert(B(5)); })
///                     .scope_target::<R1>(|_, mut ent| { ent.insert(B(6)); });
///             });
///     });
/// }
///
/// fn walk_sys(
///     roots: Query<Entity, Root<R0>>,
///     a: Query<(&A, Relations<(R0, R1)>)>,
///     b: Query<&B>,
/// ) {
///     a.ops()
///         .join::<R1>(&b)
///         .traverse::<R0>(roots.iter())
///         .for_each(|a, a_child, b| {
///             if a_child.0 == 2 {
///                 ControlFlow::Walk
///             }
///             else {
///                 println!("({}, {}, {})", a.0, a_child.0, b.0);
///                 ControlFlow::Continue
///             }
///         });
/// }
/// ```
/// ### Output of walk_sys:
/// ```ignore
///     (0, 1, 0)
///     (0, 1, 1)
/// //  Skipped:
/// //  (0, 2, 3)
/// //  (0, 2, 4)
///     (0, 3, 5)
///     (0, 3, 6)
/// ```
/// ## Probe Illustration:
/// ```
/// use bevy::prelude::*;
/// use aery::prelude::*;
///
/// #[derive(Component)]
/// struct A(usize);
///
/// #[derive(Relation)]
/// struct R;
///
/// fn setup(mut commands: Commands) {
///     commands.add(|wrld: &mut World| {
///         wrld.spawn(A(0))
///             .scope::<R>(|_, mut ent1| {
///                 ent1.insert(A(1));
///                 ent1.scope_target::<R>(|_, mut ent4| { ent4.insert(A(4)); })
///                     .scope_target::<R>(|_, mut ent5| { ent5.insert(A(5)); });
///             })
///             .scope::<R>(|_, mut ent2| {
///                 ent2.insert(A(2));
///                 ent2.scope_target::<R>(|_, mut ent6| { ent6.insert(A(6)); })
///                     .scope_target::<R>(|_, mut ent7| { ent7.insert(A(7)); });
///             })
///             .scope::<R>(|_, mut ent3| {
///                 ent3.insert(A(3));
///                 ent3.scope_target::<R>(|_, mut ent8| { ent8.insert(A(8)); })
///                     .scope_target::<R>(|_, mut ent9| { ent9.insert(A(9)); });
///             });
///     });
/// }
///
/// fn noprobe(query: Query<(&A, Relations<R>)>, roots: Query<Entity, Root<R>>) {
///     query.ops().traverse::<R>(roots.iter()).for_each(|a, a_child| {
///         // ..
///     })
/// }
///
/// fn probe(query: Query<(&A, Relations<R>)>, roots: Query<Entity, Root<R>>) {
///     query.ops().traverse::<R>(roots.iter()).for_each(|a, a_child| {
///         if (a_child.0 == 2) {
///             ControlFlow::Probe
///         }
///         else {
///             ControlFlow::Continue
///         }
///     })
/// }
/// ```
/// ### Traversal of noprobe:
/// Pink means traversed.
/// ```mermaid
/// flowchart BT
/// classDef pink fill:#f66
///
/// E1:::pink --R--> E0:::pink
/// E2:::pink --R--> E0:::pink
/// E3:::pink --R--> E0:::pink
///
/// E4:::pink --R--> E1:::pink
/// E5:::pink --R--> E1:::pink
///
/// E6:::pink --R--> E2:::pink
/// E7:::pink --R--> E2:::pink
///
/// E8:::pink --R--> E3:::pink
/// E9:::pink --R--> E3:::pink
/// ```
///
/// ### Traversal of probe:
/// Pink means traversed.
/// ```mermaid
/// flowchart BT
/// classDef pink fill:#f66
///
/// E1:::pink --R--> E0:::pink
/// E2:::pink --R--> E0:::pink
/// E3 --R--> E0:::pink
///
/// E4 --R--> E1:::pink
/// E5 --R--> E1:::pink
///
/// E6:::pink --R--> E2:::pink
/// E7:::pink --R--> E2:::pink
///
/// E8 --R--> E3
/// E9 --R--> E3
/// ```
pub enum ControlFlow {
    /// Continue to next permutation.
    Continue,
    /// Stop iterating permutatiosn and exit loop.
    Exit,
    /// FastForward(n) will advance the nth join to the next match skipping any premutations.
    /// inbetween where it currently is and the next permutation where it was supposed to advance.
    /// Has no effect for operations with no joins.
    FastForward(usize),
    /// Walks to the next entity in the "traversal" skipping any remaining permutations to iterate.
    /// - For operations with *traversals* this is the next entity in the traversal path.
    /// - Otherwise when there are only joins it's a linear traversal through the query items
    /// and this is just the next entity in the control query.
    Walk,
    /// Skips:
    /// - Any remaining join permutations.
    /// - Any remaining entities on the current breadth.
    /// - All entities in the next bredth other than the one being probed.
    Probe,
    /// Stops traversing deeper from the current node.
    Conclude,
}

impl From<()> for ControlFlow {
    fn from(_: ()) -> Self {
        ControlFlow::Continue
    }
}
