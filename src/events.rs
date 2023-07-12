use crate::{relation::RelationId, Var};

use bevy::ecs::{entity::Entity, event::Event};
use std::cmp::PartialEq;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TargetOp {
    Set,
    Unset,
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
