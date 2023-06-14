#![allow(clippy::type_complexity)]
#![allow(clippy::too_many_arguments)]

//! # Aery
//! Non-fragmenting ZST relations for Bevy featuring ergonomic query operations and cleanup
//! policies.
//! # Examples
//! ```
//! use bevy::prelude::*;
//! use aery::prelude::*;
//!
//! fn main() {
//!     App::new()
//!         .add_plugin(Aery)
//!         .add_startup_system(setup)
//!         .add_system(sys)
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
//! struct R0;
//!
//! #[derive(Relation)]
//! #[cleanup(policy = "Recursive")]
//! struct R1;
//!
//! fn setup(mut commands: Commands) {
//!     let (root, foo0, foo1, bar0, bar1) = (
//!         commands.spawn(Foo).id(),
//!         commands.spawn(Foo).id(),
//!         commands.spawn(Foo).id(),
//!         commands.spawn(Bar).id(),
//!         commands.spawn(Bar).id(),
//!     );
//!
//!     commands.set::<R0>(foo0, bar0);
//!     commands.set::<R0>(foo1, bar1);
//!     commands.set::<R1>(foo0, root);
//!     commands.set::<R1>(foo1, root);
//! }
//!
//! fn sys(
//!     foos: Query<(&Foo, Relations<(R0, R1)>)>,
//!     bars: Query<&Bar>,
//!     r1_roots: Query<Entity, Root<R1>>
//! ) {
//!     foos.ops()
//!         .join::<R0>(&bars)
//!         .breadth_first::<R1>(r1_roots.iter())
//!         .for_each(|foo_ancestor, foo, bar| {
//!             // ..
//!         })
//! }
//! ```

pub mod operations;
pub mod relation;
pub mod tuple_traits;

pub mod prelude {
    pub use crate::{
        operations::{
            AeryQueryExt, BreadthFirst, ControlFlow, ForEachPermutations,
            ForEachPermutations3Arity, Join, Relations,
        },
        relation::{CheckedDespawn, CleanupPolicy, Participates, Relation, RelationCommands, Root},
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
