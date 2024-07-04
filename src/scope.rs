use crate::{
    edges::Set,
    relation::{Relation, ZstOrPanic},
};

use bevy_ecs::{
    bundle::Bundle,
    entity::Entity,
    system::{Commands, EntityCommands},
    world::Command,
    world::{EntityWorldMut, World},
};

use std::marker::PhantomData;

/// Builder API to construct hierarchies of relations.
/// ```
/// use bevy::prelude::*;
/// use aery::prelude::*;
///
/// #[derive(Relation)]
/// struct ChildOf;
///
/// #[derive(Component)]
/// struct C<const N: usize>;
///
/// fn sys(world: &mut World) {
///     world.spawn(C::<0>)
///          .scope::<ChildOf>(|scope| {
///              // 1, 2, 3 are implicitly `ChildOf` the last spawned entity (0)
///              scope.add(C::<1>)
///                   .add(C::<2>)
///                   .add(C::<3>)
///                   .scope::<ChildOf>(|scope| {
///                        // 4, 5 are implicitly `ChildOf` the last spawned entity (3)
///                       scope.add(C::<4>)
///                            .add(C::<5>);
///                   });
///          });
/// }
/// ```
pub struct WorldScope<'a, E> {
    top: Entity,
    last: Entity,
    world: &'a mut World,
    _phantom: PhantomData<E>,
}

/// Ext trait to produce a [`WorldScope`] from an [`EntityWorldMut`].
pub trait AeryEntityWorldMutExt<'a> {
    #[allow(missing_docs)]
    fn scope<R: Relation>(&mut self, func: impl FnMut(&mut WorldScope<'_, R>)) -> &mut Self;
}

impl<'a> AeryEntityWorldMutExt<'a> for EntityWorldMut<'a> {
    fn scope<R: Relation>(&mut self, mut func: impl FnMut(&mut WorldScope<'_, R>)) -> &mut Self {
        let _ = R::ZST_OR_PANIC;

        let id = self.id();

        self.world_scope(|world| {
            let mut scope = WorldScope {
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

impl<T: Relation> WorldScope<'_, T> {
    /// Scope the last spawned entity via `R`. Any targets or hosts that are added in the scope
    /// implicitly use `R` as the edge.
    pub fn scope<R: Relation>(
        &mut self,
        mut func: impl FnMut(&mut WorldScope<'_, R>),
    ) -> &mut Self {
        let _ = R::ZST_OR_PANIC;

        let mut inner = WorldScope::<'_, R> {
            top: self.last,
            last: self.last,
            world: self.world,
            _phantom: PhantomData,
        };

        func(&mut inner);

        self
    }
}

impl<R: Relation> WorldScope<'_, R> {
    /// Spawn an entity from a bundle and have it target the currently scoped entity via `R`.
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
    pub fn add_and(&mut self, mut func: impl for<'e> FnMut(&mut EntityWorldMut<'e>)) -> &mut Self {
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
        mut func: impl for<'e> FnMut(&mut EntityWorldMut<'e>),
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

/// Builder API to construct hierarchies of relations from commands.
/// ```
/// use bevy::prelude::*;
/// use aery::prelude::*;
///
/// #[derive(Relation)]
/// struct ChildOf;
///
/// #[derive(Component)]
/// struct C<const N: usize>;
///
/// fn sys(mut cmds: Commands) {
///     cmds.spawn(C::<0>)
///         .scope::<ChildOf>(|scope| {
///             // 1, 2, 3 are implicitly `ChildOf` the last spawned entity (0)
///             scope.add(C::<1>)
///                  .add(C::<2>)
///                  .add(C::<3>)
///                  .scope::<ChildOf>(|scope| {
///                       // 4, 5 are implicitly `ChildOf` the last spawned entity (3)
///                      scope.add(C::<4>)
///                           .add(C::<5>);
///                  });
///         });
/// }
/// ```
pub struct CommandScope<'a, 'w, 's, E> {
    top: Entity,
    last: Entity,
    cmds: &'a mut Commands<'w, 's>,
    _phantom: PhantomData<E>,
}

/// Ext trait to produce a [`CommandScope`] from an [`EntityCommands`].
pub trait AeryEntityCommandsExt<'a> {
    #[allow(missing_docs)]
    fn scope<R: Relation>(
        &mut self,
        func: impl FnMut(&mut CommandScope<'_, '_, '_, R>),
    ) -> &mut Self;
}

impl<'a> AeryEntityCommandsExt<'a> for EntityCommands<'a> {
    fn scope<R: Relation>(
        &mut self,
        mut func: impl FnMut(&mut CommandScope<'_, '_, '_, R>),
    ) -> &mut Self {
        let _ = R::ZST_OR_PANIC;

        let id = self.id();

        let mut scope = CommandScope {
            top: id,
            last: id,
            cmds: &mut self.commands(),
            _phantom: PhantomData,
        };

        func(&mut scope);

        self
    }
}

impl<T: Relation> CommandScope<'_, '_, '_, T> {
    /// Scope the last spawned entity via `R`. Any targets or hosts that are added in the scope
    /// implicitly use `R` as the edge.
    pub fn scope<R: Relation>(
        &mut self,
        mut func: impl FnMut(&mut CommandScope<'_, '_, '_, R>),
    ) -> &mut Self {
        let _ = R::ZST_OR_PANIC;

        let mut inner = CommandScope::<'_, '_, '_, R> {
            top: self.last,
            last: self.last,
            cmds: self.cmds,
            _phantom: PhantomData,
        };

        func(&mut inner);

        self
    }
}

impl<R: Relation> CommandScope<'_, '_, '_, R> {
    /// Spawn an entity from a bundle and have it target the currently scoped entity via `R`.
    pub fn add(&mut self, bundle: impl Bundle) -> &mut Self {
        let id = self.cmds.spawn(bundle).id();
        self.cmds.add(Set::<R>::new(id, self.top));
        self.last = id;
        self
    }

    /// Spawn an entity from a bundle and set it as a target of the currently scoped entity.
    pub fn add_target(&mut self, bundle: impl Bundle) -> &mut Self {
        let id = self.cmds.spawn(bundle).id();
        self.cmds.add(Set::<R>::new(self.top, id));
        self.last = id;
        self
    }

    /// Spawn an entity and have it target the currently scoped entity via.
    /// This function takes a closure to provide entity mut access.
    pub fn add_and(&mut self, mut func: impl for<'a> FnMut(&mut EntityCommands<'a>)) -> &mut Self {
        let id = {
            let mut spawned = self.cmds.spawn(());
            func(&mut spawned);
            spawned.id()
        };

        self.cmds.add(Set::<R>::new(id, self.top));
        self.last = id;
        self
    }

    /// Spawn an entity and set it as a target of the currently scoped entity.
    /// This function takes a closure to provide entity mut access.
    pub fn add_target_and(
        &mut self,
        mut func: impl for<'a> FnMut(&mut EntityCommands<'a>),
    ) -> &mut Self {
        let id = {
            let mut spawned = self.cmds.spawn(());
            func(&mut spawned);
            spawned.id()
        };

        self.cmds.add(Set::<R>::new(self.top, id));
        self.last = id;
        self
    }
}
