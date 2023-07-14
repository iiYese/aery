## Aery
A plugin that adds a subset of Entity Relationship features to Bevy using Non-fragmenting
ZST relations.

[![Crates.io](https://img.shields.io/crates/v/aery)](https://crates.io/crates/aery)
[![Docs.rs](https://img.shields.io/docsrs/aery)](https://docs.rs/aery/latest/aery/)

### Currently supported:
- ZST relations
- Fragmenting on (relation) type
- Cleanup policies
- Declarative APIs for:
  - Joining
  - Traversing
  - Spawning

### API tour:
Non exhaustive. Covers most common parts.

```rust
use bevy::prelude::*;
use aery::prelude::*;

fn main() {
    App::new()
        .add_plugins(Aery)
        .add_systems(Startup, setup)
        .add_systems(Update, sys)
        .run();
}

#[derive(Component)]
struct Foo;

#[derive(Component)]
struct Bar;

#[derive(Relation)]
#[cleanup(policy = "Recursive")]
struct ChildOf;

#[derive(Relation)]
#[multi]
struct Bag;

// Spawning entities with relations
fn setup(mut commands: Commands) {
    // A hierarchy of Foos with (chocolate? OwO) Bars in their Bags
    commands.add(|wrld: &mut World| {
        wrld.spawn(Foo)
            .scope::<ChildOf>(|_, mut child| {
                child.insert(Foo);
                child.scope_target::<Bag>(|_, mut bag| { bag.insert(Bar); });
            })
            .scope::<ChildOf>(|_, mut child| {
                child.insert(Foo);
                child.scope_target::<Bag>(|_, mut bag| { bag.insert(Bar); });
            });
    })
}

// Listening for relation events
fn alert(mut events: EventReader<TargetEvent>) {
    for event in events.iter() {
        if event.matches(Wc, TargetOp::Set, ChildOf, Wc) {
            println!("{:?} was added as a child of {:?}", event.host, event.target);
        }
    }
}

// Relation Queries
fn sys(
    foos: Query<(&Foo, Relations<(Bag, ChildOf)>)>,
    roots: Query<Entity, Root<ChildOf>>,
    bars: Query<&Bar>,
) {
    foos.ops()
        .join::<Bag>(&bars)
        .traverse::<ChildOf>(roots.iter())
        .for_each(|foo_parent, foo, bar| {
            // ..
        })
}
```

### Version table
| Bevy version | Aery verison |
|--------------|--------------|
| 0.11         | 0.3          |
| 0.10         | 0.1 - 0.2    |

### Credits
- [Sander Mertens](https://github.com/SanderMertens):
Responsible for pioneering Entity Relationships in ECS and the author of Flecs which Aery has taken 
a lot of inspiration from.
