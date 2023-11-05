use crate::{
    edges::Set,
    relation::{Relation, ZstOrPanic},
};

use bevy::ecs::{
    bundle::Bundle,
    entity::Entity,
    system::{Command, Commands, EntityCommands},
    world::{EntityWorldMut, World},
};

use std::marker::PhantomData;

/// Builder API to construct hierarchies of relations.
pub struct Scope<API, E> {
    top: Entity,
    last: Entity,
    api: API,
    _phantom: PhantomData<E>,
}

impl<R: Relation> Scope<&'_ mut World, R> {
    /// Spawn an entity from a bundle and have it target the currently scoped entity via.
    pub fn add(&mut self, bundle: impl Bundle) -> &mut Self {
        let id = self.api.spawn(bundle).id();
        Command::apply(Set::<R>::new(id, self.top), self.api);
        self.last = id;
        self
    }

    /// Spawn an entity from a bundle and set it as a target of the currently scoped entity.
    pub fn add_target(&mut self, bundle: impl Bundle) -> &mut Self {
        let id = self.api.spawn(bundle).id();
        Command::apply(Set::<R>::new(self.top, id), self.api);
        self.last = id;
        self
    }

    /// Spawn an entity and have it target the currently scoped entity via.
    /// This function takes a closure to provide entity mut access.
    pub fn add_and(&mut self, mut func: impl for<'e> FnMut(&mut EntityWorldMut<'e>)) -> &mut Self {
        let id = {
            let mut spawned = self.api.spawn(());
            func(&mut spawned);
            spawned.id()
        };

        Command::apply(Set::<R>::new(id, self.top), self.api);
        self.last = id;
        self
    }

    /// Spawn an entity and set it as a target of the currently scoped entity.
    /// This function takes a closure to provide entity mut access.
    pub fn add_target_and(
        &mut self,
        mut func: impl for<'e> FnMut(&mut EntityWorldMut<'e>),
    ) -> &mut Self {
        let id = {
            let mut spawned = self.api.spawn(());
            func(&mut spawned);
            spawned.id()
        };

        Command::apply(Set::<R>::new(self.top, id), self.api);
        self.last = id;
        self
    }
}

impl<T: Relation> Scope<&'_ mut World, T> {
    /// Scope the last spawned entity via `R`. Any targets or hosts that are added in the scope
    /// implicitly use `R` as the edge.
    pub fn scope<R: Relation>(
        &mut self,
        mut func: impl for<'i> FnMut(&mut Scope<&'_ mut World, R>),
    ) -> &mut Self {
        let _ = R::ZST_OR_PANIC;

        let mut inner = Scope::<&'_ mut World, R> {
            top: self.last,
            last: self.last,
            api: self.api,
            _phantom: PhantomData,
        };

        func(&mut inner);

        self
    }
}

/// Ext trait to produce a [`Scope`] from an [`EntityWorldMut`].
pub trait AeryEntityWorldMutExt<'a> {
    #[allow(missing_docs)]
    fn scope<R: Relation>(
        &mut self,
        func: impl for<'i> FnMut(&mut Scope<&'_ mut World, R>),
    ) -> &mut Self;
}

impl<'a> AeryEntityWorldMutExt<'a> for EntityWorldMut<'a> {
    fn scope<R: Relation>(
        &mut self,
        mut func: impl for<'i> FnMut(&mut Scope<&'_ mut World, R>),
    ) -> &mut Self {
        let _ = R::ZST_OR_PANIC;

        let id = self.id();

        self.world_scope(|world| {
            let mut scope = Scope {
                top: id,
                last: id,
                api: world,
                _phantom: PhantomData,
            };

            func(&mut scope);
        });

        self
    }
}

/// Ext trait to produce a [`Scope`] from an [`EntityCommands`].
pub trait AeryEntityCommandsExt<'a> {
    #[allow(missing_docs)]
    fn scope<R: Relation>(
        &mut self,
        func: impl for<'i> FnMut(&mut Scope<&'_ mut Commands, R>),
    ) -> &mut Self;
}
