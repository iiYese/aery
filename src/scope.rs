use crate::{
    commands::{RelationCommands, Set},
    relation::{Relation, ZstOrPanic},
};

use bevy::{
    ecs::{
        bundle::Bundle,
        entity::Entity,
        system::Command,
        world::{EntityMut, World},
    },
    log::warn,
};

use std::marker::PhantomData;

pub struct Scope<'w, T = ()> {
    top: Entity,
    last: Entity,
    world: &'w mut World,
    _phantom: PhantomData<T>,
}

#[allow(clippy::should_implement_trait)]
impl<R: Relation> Scope<'_, R> {
    pub fn add(&mut self, bundle: impl Bundle) -> &mut Self {
        let spawned = self.world.spawn(bundle).id();
        Command::apply(Set::<R>::new(spawned, self.top), self.world);
        self.last = spawned;
        self
    }

    pub fn add_target(&mut self, bundle: impl Bundle) -> &mut Self {
        let spawned = self.world.spawn(bundle).id();
        Command::apply(Set::<R>::new(self.top, spawned), self.world);
        self.last = spawned;
        self
    }
}

impl<'a, T> Scope<'a, T> {
    pub fn scope<R: Relation>(
        &mut self,
        mut func: impl for<'i> FnMut(&mut Scope<'i, R>),
    ) -> &mut Self {
        let mut inner = Scope::<R> {
            top: self.last,
            last: self.last,
            world: self.world,
            _phantom: PhantomData,
        };

        func(&mut inner);

        self
    }
}

pub trait EntituMutExt<'a> {
    fn scope<R: Relation>(self, func: impl FnMut(&mut Scope<'a, R>)) -> Scope<'a>;
}

impl<'a> EntituMutExt<'a> for EntityMut<'a> {
    fn scope<R: Relation>(self, mut func: impl FnMut(&mut Scope<'a, R>)) -> Scope<'a> {
        let mut scope = Scope {
            top: self.id(),
            last: self.id(),
            world: self.into_world_mut(),
            _phantom: PhantomData,
        };

        func(&mut scope);

        Scope {
            top: scope.top,
            last: scope.last,
            world: scope.world,
            _phantom: PhantomData,
        }
    }
}
