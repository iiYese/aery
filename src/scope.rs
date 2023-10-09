use crate::{
    edges::Set,
    relation::{
        Relation,
        ZstOrPanic,
        // TODO: bevy 0.12
        // Hierarchy
    },
};

use bevy::ecs::{
    bundle::Bundle,
    entity::Entity,
    system::Command,
    world::{EntityMut, World},
};

use std::marker::PhantomData;

/// TODO: Example + mermaid illustration
/// Builder API to construct hierarchies of relations.
pub struct Scope<'w, T> {
    top: Entity,
    last: Entity,
    world: &'w mut World,
    _phantom: PhantomData<T>,
}

impl<R: Relation> Scope<'_, R> {
    /// Spawn an entity from a bundle and have it target the currently scoped entity via.
    pub fn add(&mut self, bundle: impl Bundle) -> &mut Self {
        let id = self.world.spawn(bundle).id();
        Command::apply(Set::<R>::new(id, self.top), self.world);
        self.last = id;
        self
    }

    /// Spawn an entity from a bundle and set it as a target of the currently scoped entity.
    pub fn add_target(&mut self, bundle: impl Bundle) -> &mut Self {
        let id = self.world.spawn(bundle).id();
        Command::apply(Set::<R>::new(self.top, id), self.world);
        self.last = id;
        self
    }

    /// Spawn an entity and have it target the currently scoped entity via.
    /// This function takes a closure to provide entity mut access.
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

    /// Spawn an entity and set it as a target of the currently scoped entity.
    /// This function takes a closure to provide entity mut access.
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
    /// Scope the last spawned entity via `R`. Any targets or hosts that are added in the scope
    /// implicitly use `R` as the edge.
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

// TODO: bevy 0.12
// impl<'a> Scope<'a, Hierarchy> {}

/// Ext trait to produce a [`Scope`] from an [`EntityMut`].
pub trait EntityMutExt<'a> {
    #[allow(missing_docs)]
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
