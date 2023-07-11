use crate::relation::{Relation, RelationId};

use bevy::ecs::{entity::Entity, event::Event};
use std::cmp::PartialEq;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TargetOp {
    Set,
    Unset,
}

pub enum Var<T> {
    /// Value specified.
    Val(T),
    /// Wildcard. Will match anything.
    Wc,
}

impl<T: PartialEq> PartialEq<T> for Var<T> {
    fn eq(&self, other: &T) -> bool {
        match self {
            Self::Val(v) if v == other => true,
            Self::Wc => true,
            _ => false,
        }
    }
}

impl From<Entity> for Var<Entity> {
    fn from(value: Entity) -> Self {
        Self::Val(value)
    }
}

impl<R: Relation> From<R> for Var<RelationId> {
    fn from(_: R) -> Self {
        Self::Val(RelationId::of::<R>())
    }
}

#[derive(Event, Clone, Copy, Debug)]
pub struct TargetEvent {
    pub(crate) host: Entity,
    pub(crate) target: Entity,
    pub(crate) target_op: TargetOp,
    pub(crate) relation_id: RelationId,
}

impl TargetEvent {
    pub fn matches(
        self,
        host: impl Into<Var<Entity>>,
        target_op: TargetOp,
        rel_var: impl Into<Var<RelationId>>,
        target: impl Into<Var<Entity>>,
    ) -> bool {
        host.into().eq(&self.host)
            && target_op.eq(&self.target_op)
            && rel_var.into().eq(&self.relation_id)
            && target.into().eq(&self.target)
    }
}

#[derive(Event)]
pub struct CleanupEvent {
    pub entity: Entity,
}
