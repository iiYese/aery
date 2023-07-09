use crate::relation::{Relation, RelationCommands};

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

impl Scope for EntityMut<'_> {
    fn scope<R: Relation>(&mut self, mut func: impl FnMut(EntityMut<'_>)) -> &'_ mut Self {
        let id = self.id();

        self.world_scope(|world| {
            let mut e = world.spawn_empty();
            e.set::<R>(id);
            func(e)
        });

        self
    }

    fn scope_target<R: Relation>(&mut self, mut func: impl FnMut(EntityMut<'_>)) -> &'_ mut Self {
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

impl ScopeEntity for EntityMut<'_> {
    fn scope<R: Relation>(
        &mut self,
        entity: Entity,
        mut func: impl FnMut(EntityMut<'_>),
    ) -> &'_ mut Self {
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
