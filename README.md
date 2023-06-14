## Aery
Non-fragmenting (slight misnomer) ZST relations for Bevy.

[![Crates.io](https://img.shields.io/crates/v/aery)](https://crates.io/crates/aery)
[![Docs.rs](https://img.shields.io/docsrs/aery)](https://docs.rs/aery/latest/aery/)

```rust
use bevy::prelude::*;
use aery::prelude::*;

fn main() {
    App::new()
        .add_plugin(Aery)
        .add_startup_system(setup)
        .add_system(sys)
        .run();
}

#[derive(Component)]
struct Foo;

#[derive(Component)]
struct Bar;

#[derive(Relation)]
struct R0;

#[derive(Relation)]
#[cleanup(policy = "Recursive")]
struct R1;

fn setup(mut commands: Commands) {
    let (root, foo0, foo1, bar0, bar1) = (
        commands.spawn(Foo).id(),
        commands.spawn(Foo).id(),
        commands.spawn(Foo).id(),
        commands.spawn(Bar).id(),
        commands.spawn(Bar).id(),
    );

    commands.set::<R0>(foo0, bar0);
    commands.set::<R0>(foo1, bar1);
    commands.set::<R1>(foo0, root);
    commands.set::<R1>(foo1, root);
}

fn sys(
    foos: Query<(&Foo, Relations<(R0, R1)>)>,
    bars: Query<&Bar>,
    r1_roots: Query<Entity, Root<R1>>
) {
    foos.ops()
        .join::<R0>(&bars)
        .breadth_first::<R1>(r1_roots.iter())
        .for_each(|foo_ancestor, foo, bar| {
            // ..
        })
}
```

### What is supported:
- ZST relations
- Fragmenting on (relation) type
- Declarative joining & traversing
- Explicit despawn cleanup

### What is not supported:
- Fragmenting on target
- Target querying
- Implicit despawn cleanup

### Version table
| Bevy version | Aery verison |
|--------------|--------------|
| 0.10         | 0.1 - 0.2    |
