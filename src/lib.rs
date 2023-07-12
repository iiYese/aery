#![allow(clippy::type_complexity)]
#![allow(clippy::too_many_arguments)]
#![allow(clippy::let_unit_value)]

//! # Aery
//! A plugin that adds a subset of Entity Relationship features to Bevy using Non-fragmenting
//! ZST relations.
//!
//! ### Currently supported:
//! - ZST relations
//! - Fragmenting on (relation) type
//! - Cleanup policies
//! - Declarative APIs for:
//!   - Joining
//!   - Traversing
//!   - Spawning
//!
//! # Basic:
//! ```
//! use bevy::prelude::*;
//! use aery::prelude::*;
//!
//! fn main() {
//!     App::new()
//!         .add_plugins(Aery)
//!         .add_systems(Startup, setup)
//!         .add_systems(Update, sys)
//!         .run();
//! }
//!
//! #[derive(Component)]
//! struct Foo;
//!
//! #[derive(Component)]
//! struct Bar;
//!
//! #[derive(Relation)]
//! #[cleanup(policy = "Recursive")]
//! struct Child;
//!
//! #[derive(Relation)]
//! #[multi]
//! struct Bag;
//!
//! fn setup(mut commands: Commands) {
//!     // A hierarchy of Foos with (chocolate? OwO) Bars in their Bags
//!     commands.add(|wrld: &mut World| {
//!         wrld.spawn(Foo)
//!             .scope_down::<Child>(|mut child| {
//!                 child.insert(Foo);
//!                 child.scope::<Bag>(|mut bag| { bag.insert(Bar); });
//!             })
//!             .scope_down::<Child>(|mut child| {
//!                 child.insert(Foo);
//!                 child.scope::<Bag>(|mut bag| { bag.insert(Bar); });
//!             });
//!     })
//! }
//!
//! fn sys(
//!     foos: Query<(&Foo, Relations<(Bag, Child)>)>,
//!     roots: Query<Entity, Root<Child>>
//!     bars: Query<&Bar>,
//! ) {
//!     foos.ops()
//!         .join::<Bag>(&bars)
//!         .breadth_first::<Child>(roots.iter())
//!         .for_each(|foo_parent, foo, bar| {
//!             // ..
//!         })
//! }
//! ```

pub mod commands;
pub mod events;
pub mod operations;
pub mod relation;
pub mod scope;
pub mod tuple_traits;

use commands::RefragmentHooks;
use events::{CleanupEvent, TargetEvent};
use relation::{Relation, RelationId, ZstOrPanic};

use bevy::{
    app::{App, Plugin},
    ecs::entity::Entity,
};

/// A type to enable wildcard APIs
pub enum Var<T> {
    /// Sepcific value.
    Val(T),
    /// Wildcard. Will match anything.
    Wc,
}

impl<T: PartialEq> PartialEq<T> for Var<T> {
    fn eq(&self, other: &T) -> bool {
        match self {
            Self::Val(v) if v == other => true,
            Self::Wc => true,
            _ => false,
        }
    }
}

impl From<Entity> for Var<Entity> {
    fn from(value: Entity) -> Self {
        Self::Val(value)
    }
}

impl<R: Relation> From<R> for Var<RelationId> {
    fn from(_: R) -> Self {
        let _ = R::ZST_OR_PANIC;
        Self::Val(RelationId::of::<R>())
    }
}

pub struct Aery;

impl Plugin for Aery {
    fn build(&self, app: &mut App) {
        app.init_resource::<RefragmentHooks>()
            .add_event::<TargetEvent>()
            .add_event::<CleanupEvent>();
    }
}

pub mod prelude {
    pub use super::Var::{self, Wc};
    pub use crate::{
        commands::RelationCommands,
        events::{TargetEvent, TgtOp},
        operations::{
            AeryQueryExt, ControlFlow, ForEachPermutations, ForEachPermutations3Arity, Join,
            Relations, Traverse,
        },
        relation::{CleanupPolicy, Participates, Relation, Root, ZstOrPanic},
        scope::Scope,
        tuple_traits::{Joinable, RelationSet},
        Aery,
    };
    pub use aery_macros::*;
}
