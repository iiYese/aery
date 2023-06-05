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
//!         .add_system(sys)
//!         .run();
//! }
//!
//! #[derive(Component)]
//! struct A;
//!
//! #[derive(Component)]
//! struct B;
//!
//! struct R0;
//!
//! impl Relation for R0 {}
//!
//! struct R1;
//!
//! impl Relation for R1 {}
//!
//! fn sys(left: Query<(&A, Relations<(R0, R1)>)>, b: Query<&B>, roots: Query<Entity, Root<R1>>) {
//!     left.ops()
//!         .join::<R0>(&b)
//!         .breadth_first::<R1>(roots.iter())
//!         .for_each(|a, b| {
//!             // ..
//!         })
//! }
//! ```

pub mod ops;
pub mod relation;
pub mod tuple_traits;

pub mod prelude {
    pub use crate::{
        ops::{
            AeryQueryExt, BreadthFirst, ControlFlow, InnerForEach, Join, Relations, TrappedForEach,
        },
        relation::{
            CheckedDespawn, CheckedDespawnExt, CleanupPolicy, Participates, Relation, Root, SetExt,
            UnsetExt,
        },
        tuple_traits::{Joinable, RelationSet},
        Aery,
    };
}

use bevy::app::{App, Plugin};
use relation::RefragmentHooks;

pub struct Aery;

impl Plugin for Aery {
    fn build(&self, app: &mut App) {
        app.init_resource::<RefragmentHooks>();
    }
}
