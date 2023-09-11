use crate::{
    commands::Set,
    relation::{Relation, ZstOrPanic},
};

use bevy::ecs::{
    bundle::Bundle,
    entity::Entity,
    system::Command,
    world::{EntityMut, World},
};

use std::marker::PhantomData;

pub struct Scope<'w, T: Relation> {
    top: Entity,
    last: Entity,
    world: &'w mut World,
    _phantom: PhantomData<T>,
}

#[allow(clippy::should_implement_trait)]
impl<R: Relation> Scope<'_, R> {
    pub fn add(&mut self, bundle: impl Bundle) -> &mut Self {
        let id = self.world.spawn(bundle).id();
        Command::apply(Set::<R>::new(id, self.top), self.world);
        self.last = id;
        self
    }

    pub fn add_target(&mut self, bundle: impl Bundle) -> &mut Self {
        let id = self.world.spawn(bundle).id();
        Command::apply(Set::<R>::new(self.top, id), self.world);
        self.last = id;
        self
    }

    pub fn add_and(&mut self, mut func: impl for<'e> FnMut(&mut EntityMut<'e>)) -> &mut Self {
        let id = {
            let mut spawned = self.world.spawn(());
            func(&mut spawned);
            spawned.id()
        };

        Command::apply(Set::<R>::new(id, self.top), self.world);
        self.last = id;
        self
    }

    pub fn add_target_and(
        &mut self,
        mut func: impl for<'e> FnMut(&mut EntityMut<'e>),
    ) -> &mut Self {
        let id = {
            let mut spawned = self.world.spawn(());
            func(&mut spawned);
            spawned.id()
        };

        Command::apply(Set::<R>::new(self.top, id), self.world);
        self.last = id;
        self
    }
}

impl<'a, T: Relation> Scope<'a, T> {
    pub fn scope<R: Relation>(
        &mut self,
        mut func: impl for<'i> FnMut(&mut Scope<'i, R>),
    ) -> &mut Self {
        let _ = R::ZST_OR_PANIC;

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

pub trait EntityMutExt<'a> {
    fn scope<R: Relation>(&mut self, func: impl for<'i> FnMut(&mut Scope<'i, R>)) -> &mut Self;
}

impl<'a> EntityMutExt<'a> for EntityMut<'a> {
    fn scope<R: Relation>(&mut self, mut func: impl for<'i> FnMut(&mut Scope<'i, R>)) -> &mut Self {
        let _ = R::ZST_OR_PANIC;

        let id = self.id();

        self.world_scope(|world| {
            let mut scope = Scope {
                top: id,
                last: id,
                world,
                _phantom: PhantomData,
            };

            func(&mut scope);
        });

        self
    }
}
