#![allow(clippy::type_complexity)]
#![allow(clippy::too_many_arguments)]

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
//! struct Bag;
//!
//! fn setup(mut commands: Commands) {
//!     // A hierarchy of Foos with (chocolate? OwO) Bars in their Bags
//!     commands.add(|world: &mut World| world
//!         .spawn(Foo)
//!         .scope::<Child>(|child| child
//!             .insert(Foo)
//!             .scope_target::<Bag>(|bag| bag.insert(Bar))
//!         )
//!         .scope::<Child>(|child| child
//!             .insert(Foo)
//!             .scope_target::<Bag>(|bag| bag.insert(Bar))
//!         )
//!     )
//! }
//!
//! fn sys(
//!     foos: Query<(&Foo, Relations<(Bag, Child)>)>,
//!     bars: Query<&Bar>,
//!     r1_roots: Query<Entity, Root<Child>>
//! ) {
//!     foos.ops()
//!         .join::<Bag>(&bars)
//!         .breadth_first::<Child>(r1_roots.iter())
//!         .for_each(|foo_parent, foo, bar| {
//!             // ..
//!         })
//! }
//! ```

pub mod operations;
pub mod relation;
pub mod scope;
pub mod tuple_traits;

pub mod prelude {
    pub use crate::{
        operations::{
            AeryQueryExt, BreadthFirst, ControlFlow, ForEachPermutations,
            ForEachPermutations3Arity, Join, Relations,
        },
        relation::{
            CheckedDespawn, CleanupPolicy, Participates, Relation, RelationCommands, Root,
            ZstOrPanic,
        },
        scope::{Scope, ScopeEntity},
        tuple_traits::{Joinable, RelationSet},
        Aery,
    };
    pub use aery_macros::*;
}

use bevy::app::{App, Plugin};
use relation::RefragmentHooks;

pub struct Aery;

impl Plugin for Aery {
    fn build(&self, app: &mut App) {
        app.init_resource::<RefragmentHooks>();
    }
}
