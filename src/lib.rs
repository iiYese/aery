#![allow(clippy::type_complexity)]
#![allow(clippy::needless_doctest_main)]
#![allow(clippy::too_many_arguments)]
#![allow(clippy::let_unit_value)]
#![warn(missing_docs)]

//! # Aery
//! A plugin that adds a subset of Entity Relationship features to Bevy.
//!
//! ### Currently supported:
//! - ZST edge types only (simply means edges can't hold data)
//! - Fragmenting on edge types
//! - Cleanup policies
//! - Declarative APIs for:
//!   - Joining
//!   - Traversing
//!   - Spawning
//!
//! # API tour:
//! Non exhaustive. Covers most common parts.
//! ```
//! // Modeling an item system where the enviornment can influence items.
//! use bevy::prelude::*;
//! use aery::prelude::*;
//!
//! fn main() {
//!     App::new()
//!         .add_plugins(Aery)
//!         .add_systems(Startup, setup)
//!         .add_systems(Update, (alert, sys))
//!         .run();
//! }
//!
//! #[derive(Relation)]
//! struct Inventory;
//!
//! #[derive(Component)]
//! struct Player;
//!
//! #[derive(Component)]
//! struct Freshness(f32);
//!
//! #[derive(Component)]
//! struct Raw;
//!
//! #[derive(Component)]
//! struct Cooked;
//!
//! #[derive(Component)]
//! struct Potato;
//!
//! // Climate logic
//! enum Climate {
//!     Freezing,
//!     Cold,
//!     Neutral,
//!     Hot,
//!     Blazing,
//! }
//!
//! ```

use bevy::{
    app::{App, Plugin},
    ecs::entity::Entity,
};

///
pub mod edges;
///
pub mod events;
///
pub mod for_each;
///
pub mod operations;
///
pub mod relation;
///
pub mod scope;
///
pub mod tuple_traits;

use events::{CleanupEvent, TargetEvent};

/// A type to enable wildcard APIs
pub enum Var<T> {
    /// Sepcific value.
    Val(T),
    /// Wildcard. Will match anything.
    Wc,
}

impl<T> Default for Var<T> {
    fn default() -> Self {
        Self::Wc
    }
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

impl<T> From<Option<T>> for Var<T> {
    fn from(value: Option<T>) -> Self {
        match value {
            Some(value) => Self::Val(value),
            None => Self::Wc,
        }
    }
}

impl From<Entity> for Var<Entity> {
    fn from(value: Entity) -> Self {
        Self::Val(value)
    }
}

/// Plugin that adds the resources and events created by aery.
pub struct Aery;

impl Plugin for Aery {
    fn build(&self, app: &mut App) {
        app.add_event::<TargetEvent>().add_event::<CleanupEvent>();
    }
}

///
pub mod prelude {
    #[doc(no_inline)]
    pub use super::{
        Aery,
        Var::{self, Wc},
    };
    #[doc(no_inline)]
    pub use crate::{
        edges::{Abstains, Branch, Leaf, Participates, RelationCommands, Root, Set, Unset},
        events::{CleanupEvent, Op, TargetEvent},
        for_each::*,
        operations::{
            utils::{EdgeSide, Relations, Up},
            FoldBreadth, Join, Track, TrackSelf, Traverse,
        },
        relation::{CleanupPolicy, Relation, ZstOrPanic},
        scope::{EntityMutExt, Scope},
        tuple_traits::{Joinable, RelationSet},
    };
    #[doc(no_inline)]
    pub use aery_macros::*;
}
