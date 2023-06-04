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

struct R0;

impl Relation for R0 {}

struct R1;

impl Relation for R1 {}

fn sys(left: Query<(&A, Relations<(R0, R1)>)>, b: Query<&B>, roots: Query<Entity, Root<R1>>) {
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
