#![allow(clippy::type_complexity)]
#![allow(clippy::needless_doctest_main)]
#![allow(clippy::too_many_arguments)]
#![allow(clippy::let_unit_value)]
#![warn(missing_docs)]

//! ## Aery
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
//! Non exhaustive. Covers most common parts. It's modeling RPG mechanics resembling tears of the
//! kingdom (please nintendo leave me along I beg).
//!
//! <details>
//! <summary>Boiler plate</summary>
//!
//! ```ignore
//! use bevy::prelude::*;
//! use aery::prelude::*;
//!
//! #[derive(Clone, Copy, Component)]
//! struct Pos(Vec3);
//!
//! #[derive(Component)]
//! struct Character;
//!
//! #[derive(Component)]
//! struct Weapon {
//!     uses: u32,
//!     strength: u32,
//! }
//!
//! #[derive(Component)]
//! struct Stick;
//!
//! #[derive(Clone, Copy)]
//! enum Climate {
//!     Freezing,
//!     Cold,
//!     Neutral,
//!     Hot,
//!     Blazing,
//! }
//!
//! #[derive(Resource)]
//! struct ClimateMap {
//!     // ..
//! }
//!
//! impl ClimateMap {
//!     fn climate_at(&self, pos: Pos) -> Climate {
//!         todo!()
//!     }
//! }
//!
//! #[derive(Component)]
//! enum Food {
//!     Raw { freshness: f32 },
//!     Cooked,
//!     Spoiled,
//! }
//!
//! impl Food {
//!     fn tick(&mut self, climate: Climate) {
//!         let Food::Raw { freshness } = self else { return };
//!
//!         if *freshness < 0. {
//!             *self = Food::Spoiled;
//!             return
//!         }
//!
//!         match climate {
//!             Climate::Neutral => *freshness -= 1.,       // spoils over time
//!             Climate::Cold => *freshness -= 0.1,         // spoils slowly
//!             Climate::Freezing => *freshness -= 0.01,    // spoils very slowly
//!             Climate::Hot => *freshness -= 5.,           // spoils quickly
//!             Climate::Blazing => *self = Food::Cooked,   // Cooks food (should add a timer)
//!         }
//!     }
//! }
//!
//! #[derive(Component)]
//! struct Apple;
//! ```
//!
//! </details>
//!
//! <details>
//! <summary>Modeling a player inventory (Making relations)</summary>
//!
//! ```ignore
//! #[derive(Relation)]
//! struct Inventory;
//!
//! fn setup(mut cmds: Commands) {
//!     // Spawn character with some starting items.
//!     cmds.spawn((Character, Pos(Vec3::default())))
//!         .scope::<Inventory>(|invt| {
//!             // Give them a starting weapon & 3 food items
//!             invt.add((Weapon { uses: 32, strength: 4 }, Stick))
//!                 .add((Food::Raw { freshness: 128. }, Apple))
//!                 .add((Food::Raw { freshness: 128. }, Apple))
//!                 .add((Food::Raw { freshness: 128. }, Apple));
//!         });
//!
//!     // Alternatively construct relatiosn manually.
//!     // This might be more appropriate for changing an inventory or making more complex graphs.
//!     let char = cmds.spawn((Character, Pos(Vec3::default()))).id();
//!     cmds.spawn((Weapon { uses: 32, strength: 4, }, Stick)).set::<Inventory>(char);
//!     cmds.spawn((Food::Raw { freshness: 128. }, Apple)).set::<Inventory>(char);
//!     cmds.spawn((Food::Raw { freshness: 128. }, Apple)).set::<Inventory>(char);
//!     cmds.spawn((Food::Raw { freshness: 128. }, Apple)).set::<Inventory>(char);
//! }
//! ```
//!
//! </details>
//!
//! <details>
//! <summary>Making items respond to enviornment (Join operations)</summary>
//!
//! ```ignore
//! fn tick_food(
//!     mut characters: Query<((&Character, &Pos), Relations<Inventory>)>,
//!     mut inventory_food: Query<&mut Food, Without<Pos>>,
//!     mut food: Query<(&mut Food, &Pos)>,
//!     climate_map: Res<ClimateMap>,
//! ) {
//!     // Tick foods that are just in the world somewhere
//!     for (mut food, pos) in food.iter_mut() {
//!         food.tick(climate_map.climate_at(*pos));
//!     }
//!
//!     // Tick foods that are in a character's inventory based on the character's position
//!     for ((_, pos), edges) in characters.iter() {
//!         let climate = climate_map.climate_at(*pos);
//!         edges.join::<Inventory>(&mut inventory_food).for_each(|mut food| {
//!             food.tick(climate);
//!         });
//!     }
//! }
//! ```
//!
//! </details>
//!
//! <details>
//! <summary>Dropping inventory items into the world (Responding to relation changes)</summary>
//!
//! ```ignore
//! fn drop_item_from_inventory(
//!     mut trigger: Trigger<UnsetEvent<Inventory>>,
//!     mut commands: Commands,
//!     characters: Query<&Pos, With<Character>>,
//!     food: Query<Entity, With<Food>>,
//! ) {
//!     // Set an items position to the position of the character that last had the item
//!     // in their inventory when they drop it.
//!     let Ok(pos) = characters.get(trigger.event().target) else { return };
//!     commands.entity(trigger.entity()).insert(*pos);
//! }
//! ```
//!
//! </details>
//!
//! <details>
//! <summary>Powering connected devices (Traversing relations & relation properties)</summary>
//!
//! ```ignore
//! // This relation has a custom property. Properties can be overriden by supplying arguments to
//! // the attribute macro. See the `Relation` trait & `CleanupPolicy` enum for more details.
//! // - Symmetric: Makes relations symmetric. Setting A -R-> B also sets B -R-> A.
//! // - Poly: Allows holding multiple relations of that type to different entities.
//! //
//! // There are also cleanup properties. Only one of these can be supplied to the attribute macro.
//! // - Counted: Edge counted cleanup (eg. despawn a parent if all its children are despawned)
//! // - Recursive: Recursively cleans up (eg. despawn all children of a parent with the parent)
//! // - Total: Does both counted & recursive cleanup
//! #[derive(Relation)]
//! #[aery(Symmetric, Poly)]
//! struct FuseJoint;
//!
//! #[derive(Component)]
//! struct Fan {
//!     orientation: Quat
//! }
//!
//! #[derive(Component)]
//! struct Powered;
//!
//! fn tick_devices(
//!     mut devices: Query<((Entity, &mut Pos), Relations<FuseJoint>)>,
//!     mut fans: Query<(Entity, &Fan, &mut Pos), With<Powered>>,
//! ) {
//!     for (entity, fan, pos) in fans.iter_mut() {
//!         // Move the fan based on its orientation
//!         pos = todo!();
//!
//!         // Track visited nodes because this is a symmetric relationship
//!         let mut updated = vec![entity];
//!
//!         devices.traverse_mut::<FuseJoint>([entity]).for_each(|(entity, ref mut pos), _| {
//!             if updated.contains(&entity) {
//!                 TCF::Close
//!             } else {
//!                 // Move connected device based on fan direction
//!                 pos = todo!();
//!                 updated.push(*entity);
//!                 TCF::Continue
//!             }
//!         });
//!     }
//! }
//! ```
//!
//! </details>

#[allow(missing_docs)]
pub mod edges;
#[allow(missing_docs)]
pub mod for_each;
#[allow(missing_docs)]
pub mod operations;
#[allow(missing_docs)]
pub mod relation;
#[allow(missing_docs)]
pub mod scope;
#[allow(missing_docs)]
pub mod tuple_traits;

#[allow(missing_docs)]
pub mod prelude {
    #[doc(no_inline)]
    #[doc(no_inline)]
    pub use crate::{
        edges::{
            Abstains, Branch, Leaf, Participates, RelationCommands, Root, Set, SetEvent, Unset,
            UnsetEvent,
        },
        for_each::*,
        operations::{
            utils::{EdgeSide, Relations, Up},
            FoldBreadth, Join, Track, TrackSelf, Traverse,
        },
        relation::{CleanupPolicy, Hierarchy, RegisterRelation, Relation, ZstOrPanic},
        scope::{AeryEntityCommandsExt, AeryEntityWorldMutExt},
        tuple_traits::{Joinable, RelationSet},
    };
    #[doc(no_inline)]
    pub use aery_macros::*;
}
