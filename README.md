## Aery
A plugin that adds a subset of Entity Relationship features to Bevy.

[![Crates.io](https://img.shields.io/crates/v/aery)](https://crates.io/crates/aery)
[![Docs.rs](https://img.shields.io/docsrs/aery)](https://docs.rs/aery/latest/aery/)

### Currently supported:
- ZST edge types only (simply means edges can't hold data)
- Fragmenting on edge types
- Cleanup policies
- Declarative APIs for:
  - Joining
  - Traversing
  - Spawning

# API tour:
Non exhaustive. Covers most common parts.
```rust
// Modeling RPG mechanics that resemble TOTK:
// - Items interacting with enviornment climate
// - Powering connected devices
use bevy::prelude::*;
use aery::prelude::*;

#[derive(Clone, Copy, Component)]
struct Pos(Vec3);

#[derive(Component)]
struct Character;

#[derive(Clone, Copy)]
enum Climate {
    Freezing,
    Cold,
    Neutral,
    Hot,
    Blazing,
}

#[derive(Resource)]
struct ClimateMap {
    // ..
}

impl ClimateMap {
    fn climate_at(&self, pos: Pos) -> Climate {
        todo!()
    }
}

#[derive(Component)]
enum Food {
    Raw { freshness: f32 },
    Cooked,
    Spoiled,
}

impl Food {
    fn tick(&mut self, climate: Climate) {
        let Food::Raw { freshness } = self else { return };

        if *freshness < 0. {
            *self = Food::Spoiled;
            return
        }

        match climate {
            Climate::Neutral => *freshness -= 1.,       // spoils over time
            Climate::Cold => *freshness -= 0.1,         // spoils slowly
            Climate::Freezing => *freshness -= 0.01,    // spoils very slowly
            Climate::Hot => *freshness -= 5.,           // spoils quickly
            Climate::Blazing => *self = Food::Cooked,   // Cooks food (should add a timer)
        }
    }
}

#[derive(Relation)]
struct Inventory;

fn tick_food(
    mut characters: Query<((&Character, &Pos), Relations<Inventory>)>,
    mut inventory_food: Query<&mut Food, Without<Pos>>,
    mut food: Query<(&mut Food, &Pos)>,
    climate_map: Res<ClimateMap>,
) {
    // Tick foods that are just in the world somewhere
    for (mut food, pos) in food.iter_mut() {
        food.tick(climate_map.climate_at(*pos));
    }

    // Tick foods that are in a character's inventory based on the character's position
    for ((_, pos), edges) in characters.iter() {
        let climate = climate_map.climate_at(*pos);
        edges.join::<Inventory>(&mut inventory_food).for_each(|mut food| {
            food.tick(climate);
        });
    }
}

fn drop_item_from_inventory(
    mut commands: Commands,
    mut events: EventReader<TargetEvent>,
    characters: Query<&Pos, With<Character>>,
) {
    // Set an items position to the position of the character that last had the item
    // in their inventory when they drop it.
    for event in events
        .iter()
        .filter(|event| event.matches(Wc, Op::Unset, Inventory, Wc))
    {
        let Ok(pos) = characters.get(event.target) else { return };
        commands.entity(event.host).insert(*pos);
    }

}

#[derive(Relation)]
#[aery(Symmetric)]
struct FuseJoint;

#[derive(Component)]
struct Fan {
    orientation: Quat
}

#[derive(Component)]
struct Powered;

fn tick_devices(
    mut devices: Query<((Entity, &mut Pos), Relations<FuseJoint>)>,
    mut fans: Query<(Entity, &Fan, &mut Pos), With<Powered>>,
) {
    for (entity, fan, pos) in fans.iter_mut() {
        // Move the fan based on its orientation
        pos = todo!();

        // Track visited nodes because this is a symmetric relationship
        let mut updated = vec![entity];

        devices.traverse_mut::<FuseJoint>([entity]).for_each(|(entity, ref mut pos), _| {
            if updated.contains(&entity) {
                TCF::Close
            } else {
                // Move connected device based on fan direction
                pos = todo!();
                updated.push(*entity);
                TCF::Continue
            }
        });
    }
}
```

### Version table
| Bevy version | Aery verison |
|--------------|--------------|
| 0.11         | 0.4          |
| 0.11         | 0.3          |
| 0.10         | 0.1 - 0.2    |

### Credits
- [Sander Mertens](https://github.com/SanderMertens):
Responsible for pioneering Entity Relationships in ECS and the author of Flecs which Aery has taken 
a lot of inspiration from.
