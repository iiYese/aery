use crate::{relation::RelationId, Var};

use bevy::ecs::{entity::Entity, event::Event};
use std::cmp::PartialEq;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TgtOp {
    Set,
    Unset,
}

/// An event to notify when a target has changed for a relation.
#[derive(Event, Clone, Copy, Debug)]
pub struct TargetEvent {
    pub host: Entity,
    pub target: Entity,
    pub target_op: TgtOp,
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
    ///     mut evr: EventReader<TargetEvent>,
    ///     a: Query<(Entity, &A)>,
    ///     b: Query<(Entity, &B)>,
    /// ) {
    ///     let (foo, _) = a.single();
    ///     let (bar, _) = b.single();
    ///
    ///     for event in evr.iter() {
    ///         // Anything Set to anything else
    ///         if event.matches(Wc, TgtOp::Set, Wc, Wc) {
    ///             if let Ok(a) = a.get(event.host) {
    ///                 // Do something if it was a host with an `A` component
    ///             }
    ///
    ///             if let Ok(a) = a.get(event.target) {
    ///                 // Do something if it was a target with an `A` component
    ///             }
    ///         }
    ///
    ///         // Anything Unset to anything else
    ///         if event.matches(Wc, TgtOp::Unset, Wc, Wc) {
    ///             if let Ok(a) = a.get(event.host) {
    ///                 // Do something if it was a host with an `A` component
    ///             }
    ///
    ///             if let Ok(a) = a.get(event.target) {
    ///                 // Do something if it was a target with an `A` component
    ///             }
    ///         }
    ///
    ///         // Anything Set an `R` to anything else
    ///         if event.matches(Wc, TgtOp::Set, R, Wc) {
    ///             // ..
    ///         }
    ///
    ///         // foo Set anything to anything
    ///         if event.matches(foo, TgtOp::Set, Wc, Wc) {
    ///             // ..
    ///         }
    ///
    ///         // foo Set an `R` to anything
    ///         if event.matches(foo, TgtOp::Set, R, Wc) {
    ///             // ..
    ///         }
    ///
    ///         // foo Set anything to bar
    ///         if event.matches(foo, TgtOp::Set, Wc, bar) {
    ///             // ..
    ///         }
    ///
    ///         // foo Set an `R` to bar
    ///         if event.matches(foo, TgtOp::Set, R, bar) {
    ///             // ..
    ///         }
    ///     }
    /// }
    /// ```
    pub fn matches(
        self,
        host: impl Into<Var<Entity>>,
        target_op: TgtOp,
        rel_var: impl Into<Var<RelationId>>,
        target: impl Into<Var<Entity>>,
    ) -> bool {
        host.into().eq(&self.host)
            && target_op.eq(&self.target_op)
            && rel_var.into().eq(&self.relation_id)
            && target.into().eq(&self.target)
    }
}

/// An event to notify when an entity was despawned as the resultof a cleanup policy.
#[derive(Event)]
pub struct CleanupEvent {
    pub entity: Entity,
}
