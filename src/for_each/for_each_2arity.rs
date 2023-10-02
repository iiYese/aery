use crate::tuple_traits::*;
use crate::{
    for_each::ControlFlow,
    operations::utils::{EdgeSide, Operations, Relations, RelationsItem, SelfTracking},
};

use bevy::ecs::{
    entity::Entity,
    query::{ReadOnlyWorldQuery, WorldQuery},
    system::Query,
};

use std::{borrow::Borrow, collections::VecDeque};

// ------------------------
// - Traversal
// - Self tracking
// ------------------------

impl<Q, RS, F, T> ForEachPermutations2Arity<0>
    for Operations<&'_ Query<'_, '_, (Q, Relations<RS>), F>, (), (), T, Entity, (), SelfTracking>
where
    Q: WorldQuery,
    RS: RelationSet,
    F: ReadOnlyWorldQuery,
    T: EdgeSide,
    for<'i> RelationsItem<'i, RS>: RelationEntries,
{
    type P0<'p0> = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'p0>;
    type P1<'p1> = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'p1>;

    fn for_each<Func, Ret>(self, mut func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'p0, 'p1> FnMut(&'f mut Self::P0<'p0>, Self::P1<'p1>) -> Ret,
    {
        let mut queue = VecDeque::from([self.start]);

        'queue: while let Some(entity) = queue.pop_front() {
            let Ok((mut control, relations)) = self.control.get(entity) else {
                continue;
            };

            for e in T::entities(&relations) {
                let Ok(traversal_item) = self.control.get(e) else {
                    continue;
                };

                match func(&mut control, traversal_item.0).into() {
                    ControlFlow::Exit => return,
                    ControlFlow::Conclude => {
                        continue 'queue;
                    }
                    ControlFlow::Probe => {
                        queue.clear();
                        queue.push_back(e);
                        continue 'queue;
                    }
                    _ => {}
                }
            }

            queue.extend(T::entities(&relations));
        }
    }
}

impl<Q, RS, F, T> ForEachPermutations2Arity<0>
    for Operations<
        &'_ mut Query<'_, '_, (Q, Relations<RS>), F>,
        (),
        (),
        T,
        Entity,
        (),
        SelfTracking,
    >
where
    Q: WorldQuery,
    RS: RelationSet,
    F: ReadOnlyWorldQuery,
    T: EdgeSide,
    for<'i> RelationsItem<'i, RS>: RelationEntries,
{
    type P0<'p0> = <Q as WorldQuery>::Item<'p0>;
    type P1<'p1> = <Q as WorldQuery>::Item<'p1>;

    fn for_each<Func, Ret>(self, mut func: Func)
    where
        Ret: Into<ControlFlow>,
        Func: for<'f, 'p0, 'p1> FnMut(&'f mut Self::P0<'p0>, Self::P1<'p1>) -> Ret,
    {
        let mut queue = VecDeque::from([self.start]);

        'queue: while let Some(entity) = queue.pop_front() {
            // SAFETY: Self referential relations are impossible so this is always safe.
            let Ok((mut control, relations)) = (unsafe { self.control.get_unchecked(entity) })
            else {
                continue;
            };

            for e in T::entities(&relations) {
                // SAFETY: Self referential relations are impossible so this is always safe.
                let Ok(traversal_item) = (unsafe { self.control.get_unchecked(e) }) else {
                    continue;
                };

                match func(&mut control, traversal_item.0).into() {
                    ControlFlow::Exit => return,
                    ControlFlow::Conclude => {
                        continue 'queue;
                    }
                    ControlFlow::Probe => {
                        queue.clear();
                        queue.push_back(e);
                        continue 'queue;
                    }
                    _ => {}
                }
            }

            queue.extend(T::entities(&relations));
        }
    }
}
