use crate::{relation::RelationId, Var};

use bevy::ecs::{entity::Entity, event::Event};
use std::cmp::PartialEq;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Op {
    Set,
    Unset,
}

impl From<Op> for Var<Op> {
    fn from(op: Op) -> Self {
        Self::Val(op)
    }
}

/// An event to notify when a target has changed for a relation.
#[derive(Event, Clone, Copy, Debug)]
pub struct TargetEvent {
    pub host: Entity,
    pub target: Entity,
    pub target_op: Op,
    pub relation_id: RelationId,
}

impl TargetEvent {
    /// Function to check if an event meets a critera.
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
    /// fn sys(
    ///     mut events: EventReader<TargetEvent>,
    ///     a: Query<(Entity, &A)>,
    ///     b: Query<(Entity, &B)>,
    /// ) {
    ///     let (foo, _) = a.single();
    ///     let (bar, _) = b.single();
    ///
    ///     for event in events.iter() {
    ///         // Anything Set anything to anything else
    ///         if event.matches(Wc, Op::Set, Wc, Wc) {
    ///             if let Ok(a) = a.get(event.host) {
    ///                 // Do something if it was a host with an `A` component
    ///             }
    ///
    ///             if let Ok(a) = a.get(event.target) {
    ///                 // Do something if it was a target with an `A` component
    ///             }
    ///         }
    ///
    ///         // foo Set an `R` to bar
    ///         if event.matches(foo, Op::Set, R, bar) {
    ///             // ..
    ///         }
    ///
    ///         // foo Set an `R` to something
    ///         if event.matches(foo, Op::Set, R, Wc) {
    ///             // ..
    ///         }
    ///
    ///         // foo Set something to something
    ///         if event.matches(foo, Op::Set, Wc, Wc) {
    ///             // ..
    ///         }
    ///
    ///         // foo did anything
    ///         if event.matches(foo, Wc, Wc, Wc) {
    ///             // ..
    ///         }
    ///
    ///         // this is useless lol
    ///         if event.matches(Wc, Wc, Wc, Wc) {
    ///             // ..
    ///         }
    ///     }
    /// }
    /// ```
    pub fn matches(
        self,
        host: impl Into<Var<Entity>>,
        target_op: impl Into<Var<Op>>,
        rel_var: impl Into<Var<RelationId>>,
        target: impl Into<Var<Entity>>,
    ) -> bool {
        host.into().eq(&self.host)
            && target_op.into().eq(&self.target_op)
            && rel_var.into().eq(&self.relation_id)
            && target.into().eq(&self.target)
    }
}

/// An event to notify when an entity was despawned as the resultof a cleanup policy.
#[derive(Event)]
pub struct CleanupEvent {
    pub entity: Entity,
}
