use crate::{
    commands::{RelationCommands, Set},
    relation::{Relation, ZstOrPanic},
};

use bevy::{
    ecs::{entity::Entity, system::Command, world::EntityMut},
    log::warn,
};

use std::marker::PhantomData;

/// An extension API for `EntityMut<'_>` to make spawning and changing relation graphs easier.
/// All functions will set relations if they do not exist including overwriting exclusive relations!
/// Since changing relations can trigger cleanup procedures that might despawn the `Entity` referred
/// to by `EntytMut<'_>` each method is consuming and returns an `Option<EntityMut<'_>>`.
///
/// For convenience `Scope<'_>` is also implemented for `Option<EntityMut<'_>>`. Where the methods
/// are essentially their non-option equivalent wrapped in an implicit [`Option::and_then`] call
/// that emits warnings when the option is `None`.
pub trait Scope<'a>: Sized {
    // DEV NOTE:
    //
    //  Will always return `Some` for Slef = `EntityMut` but not for Self = `Option<EntityMut<'_>>`
    //  Can make generic programming slightly annoying to reflect this in the API so is left as is.
    //
    /// Spawns a host and gives mutable access to it via `EntityMut<'_>`.
    /// Hierarchies are bottom up so the host is also the descendant.
    fn scope<R: Relation>(self, func: impl FnMut(Entity, EntityMut<'_>)) -> Option<EntityMut<'a>>;

    /// Spawns a target and gives mutable access to it via `EntityMut<'_>`.
    /// Hierarchies are bottom up so the target is also the ancestor.
    fn scope_target<R: Relation>(
        self,
        func: impl FnMut(Entity, EntityMut<'_>),
    ) -> Option<EntityMut<'a>>;

    /// Tries to scope an existing entity as a host. Gives mutable access via `EntityMut<'_>`.
    /// A warning is emitted when the entity doesn't exist.
    /// Hierarchies are bottom up so the host is also the descendant.
    fn scope_entity<R: Relation>(
        self,
        entity: Entity,
        func: impl FnMut(EntityMut<'_>),
    ) -> Option<EntityMut<'a>>;

    /// Tries to scope an existing entity as a target. Gives mutable access via `EntityMut<'_>`.
    /// A warning is emitted when the entity doesn't exist.
    /// Hierarchies are bottom up so the target is also the ancestor.
    fn scope_target_entity<R: Relation>(
        self,
        entity: Entity,
        func: impl FnMut(EntityMut<'_>),
    ) -> Option<EntityMut<'a>>;
}

impl<'a> Scope<'a> for EntityMut<'a> {
    fn scope<R: Relation>(self, mut func: impl FnMut(Entity, EntityMut<'_>)) -> Option<Self> {
        let _ = R::ZST_OR_PANIC;

        let target = self.id();
        let world = self.into_world_mut();
        let host = world.spawn_empty();

        if let Some(host) = host.set::<R>(target) {
            func(target, host);
        }

        world.get_entity_mut(target)
    }

    fn scope_target<R: Relation>(
        self,
        mut func: impl FnMut(Entity, EntityMut<'_>),
    ) -> Option<Self> {
        let _ = R::ZST_OR_PANIC;

        let host = self.id();
        let world = self.into_world_mut();
        let target = world.spawn_empty().id();

        Command::apply(
            Set::<R> {
                host,
                target,
                _phantom: PhantomData,
            },
            world,
        );

        if let Some(target) = world.get_entity_mut(target) {
            func(host, target);
        }

        world.get_entity_mut(host)
    }

    fn scope_target_entity<R: Relation>(
        self,
        entity: Entity,
        mut func: impl FnMut(EntityMut<'_>),
    ) -> Option<Self> {
        let _ = R::ZST_OR_PANIC;

        let host = self.id();
        let world = self.into_world_mut();

        Command::apply(
            Set::<R> {
                host,
                target: entity,
                _phantom: PhantomData,
            },
            world,
        );

        if let Some(target) = world.get_entity_mut(entity) {
            func(target)
        } else {
            warn!(
                "Tried to scope {:?} as a target which does not exist. Ignoring.",
                entity
            );
        };

        world.get_entity_mut(host)
    }

    fn scope_entity<R: Relation>(
        self,
        entity: Entity,
        mut func: impl FnMut(EntityMut<'_>),
    ) -> Option<Self> {
        let _ = R::ZST_OR_PANIC;

        let target = self.id();
        let world = self.into_world_mut();

        if let Some(host) = world
            .get_entity_mut(entity)
            .and_then(|host| host.set::<R>(target))
        {
            func(host);
        } else {
            warn!(
                "Tried to scope {:?} as a host which does not exist. Ignoring.",
                entity
            );
        }

        world.get_entity_mut(target)
    }
}

impl<'a> Scope<'a> for Option<EntityMut<'a>> {
    fn scope_target<R: Relation>(self, func: impl FnMut(Entity, EntityMut<'_>)) -> Self {
        match self {
            Some(entity_mut) => entity_mut.scope_target::<R>(func),
            None => {
                warn!("Tried to scope from an optional entity that doesn't exist. Ignoring.",);
                None
            }
        }
    }

    fn scope<R: Relation>(self, func: impl FnMut(Entity, EntityMut<'_>)) -> Self {
        match self {
            Some(entity_mut) => entity_mut.scope::<R>(func),
            None => {
                warn!("Tried to scope from an optional entity that doesn't exist. Ignoring.",);
                None
            }
        }
    }

    fn scope_target_entity<R: Relation>(
        self,
        entity: Entity,
        func: impl FnMut(EntityMut<'_>),
    ) -> Self {
        match self {
            Some(entity_mut) => entity_mut.scope_target_entity::<R>(entity, func),
            None => {
                warn!("Tried to scope from an optional entity that doesn't exist. Ignoring.",);
                None
            }
        }
    }

    fn scope_entity<R: Relation>(self, entity: Entity, func: impl FnMut(EntityMut<'_>)) -> Self {
        match self {
            Some(entity_mut) => entity_mut.scope_entity::<R>(entity, func),
            None => {
                warn!("Tried to scope from an optional entity that doesn't exist. Ignoring.",);
                None
            }
        }
    }
}
