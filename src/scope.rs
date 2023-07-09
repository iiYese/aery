use crate::relation::{Relation, RelationCommands, ZstOrPanic};

use bevy::{
    ecs::{entity::Entity, world::EntityMut},
    log::warn,
};

pub trait Scope {
    fn scope<R: Relation>(&mut self, func: impl FnMut(EntityMut<'_>)) -> &'_ mut Self;
    fn scope_target<R: Relation>(&mut self, func: impl FnMut(EntityMut<'_>)) -> &'_ mut Self;
}

pub trait ScopeEntity {
    fn scope<R: Relation>(
        &mut self,
        entity: Entity,
        func: impl FnMut(EntityMut<'_>),
    ) -> &'_ mut Self;

    fn scope_target<R: Relation>(
        &mut self,
        entity: Entity,
        func: impl FnMut(EntityMut<'_>),
    ) -> &'_ mut Self;
}

#[allow(clippy::let_unit_value)]
impl Scope for EntityMut<'_> {
    fn scope<R: Relation>(&mut self, mut func: impl FnMut(EntityMut<'_>)) -> &'_ mut Self {
        let _ = R::ZST_OR_PANIC;

        let id = self.id();

        self.world_scope(|world| {
            let mut e = world.spawn_empty();
            e.set::<R>(id);
            func(e)
        });

        self
    }

    fn scope_target<R: Relation>(&mut self, mut func: impl FnMut(EntityMut<'_>)) -> &'_ mut Self {
        let _ = R::ZST_OR_PANIC;

        let mut id = Entity::PLACEHOLDER;

        self.world_scope(|world| {
            let e = world.spawn_empty();
            id = e.id();
            func(e);
        });

        self.set::<R>(id);

        self
    }
}

#[allow(clippy::let_unit_value)]
impl ScopeEntity for EntityMut<'_> {
    fn scope<R: Relation>(
        &mut self,
        entity: Entity,
        mut func: impl FnMut(EntityMut<'_>),
    ) -> &'_ mut Self {
        let _ = R::ZST_OR_PANIC;

        let id = self.id();

        self.world_scope(|world| {
            let Some(mut e) = world.get_entity_mut(entity) else {
                warn!("Tried to scope {:?} which does not exist. Ignoring.", entity);
                return
            };

            e.set::<R>(id);
            func(e);
        });

        self
    }

    fn scope_target<R: Relation>(
        &mut self,
        entity: Entity,
        mut func: impl FnMut(EntityMut<'_>),
    ) -> &'_ mut Self {
        let _ = R::ZST_OR_PANIC;

        let mut exists = true;

        self.world_scope(|world| {
            let Some(e) = world.get_entity_mut(entity) else {
                exists = false;
                warn!("Tried to scope {:?} as a target which does not exist. Ignoring.", entity);
                return
            };

            func(e);
        });

        if exists {
            self.set::<R>(entity);
        }

        self
    }
}
