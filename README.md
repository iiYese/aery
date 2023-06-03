## Aery
Non-fragmenting (slight misnomer) ZST relations for Bevy.

```rs
use bevy::prelude::*;
use aery::prelude::*;

fn main() {
    App::new()
        .add_plugin(Aery)
        .add_system(sys)
        .run();
}

#[derive(Component)]
struct A;

#[derive(Component)]
struct B;

#[derive(Relation)]
struct R0;

#[derive(Relation)]
struct R1;

fn sys(left: Query<(&A, Relations<(R0, R1)>)>, b: Query<&B>, roots: Query<Entity, RootOf<R1>>) {
    left.ops()
        .join::<R0>(&b)
        .breadth_first::<R1>(roots.iter())
        .for_each(|a, b| {
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
