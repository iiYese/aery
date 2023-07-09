use crate::relation::{CheckedDespawn, Relation, Set, Unset, UnsetAll, Withdraw};
use std::marker::PhantomData;

use bevy::ecs::{
    bundle::Bundle,
    entity::Entity,
    system::{Command, Commands},
    world::{EntityMut, EntityRef, World},
};

pub trait Scope {
    fn scope<R: Relation>(self, func: impl FnMut(EntityMut<'_>));
    fn scope_target<R: Relation>(self, func: impl FnMut(EntityMut<'_>));
}

pub trait ScopeEntity {
    fn scope<R: Relation>(self, entity: Entity, func: impl FnMut(EntityMut<'_>));
    fn scope_target<R: Relation>(self, entity: Entity, func: impl FnMut(EntityMut<'_>));
}
