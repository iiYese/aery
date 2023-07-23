use aery::prelude::*;
use bevy::prelude::*;
use criterion::{criterion_group, criterion_main, Criterion};

#[derive(Relation)]
#[cleanup(policy = "Recursive")]
struct ChildOf;

fn create_delete_tree_hierarchy(mut commands: Commands, depth: usize, width: usize) {
    commands.add(move |world: &mut World| {
        let root = world.spawn_empty().id();
        let mut cur = root;

        for _ in 0..depth {
            for _ in 0..width - 1 {
                let child = world.spawn_empty().id();
                world.entity_mut(cur).add_child(child);
            }
            let child = world.spawn_empty().id();
            world.entity_mut(cur).add_child(child);
            cur = child;
        }
        world.entity_mut(root).despawn_recursive();
    });
}

fn create_delete_tree_aery(mut commands: Commands, depth: usize, width: usize) {
    commands.add(move |world: &mut World| {
        let root = world.spawn_empty().id();
        let mut cur = root;

        for _ in 0..depth {
            for _ in 0..width - 1 {
                world.spawn_empty().set::<ChildOf>(cur);
            }
            cur = world.spawn_empty().set::<ChildOf>(cur).unwrap().id();
        }
        world.entity_mut(root).checked_despawn();
    });
}

pub fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("flecs_create_delete_tree_aery");
    for depth in [1, 10, 100, 1000] {
        for width in [1, 10, 100, 1000] {
            let mut app = App::new();
            app.add_plugins(Aery)
                .add_systems(Update, move |commands: Commands| {
                    create_delete_tree_aery(commands, depth, width)
                });
            group.bench_with_input(format!("d{}w{}", depth, width), &(depth, width), |b, _| {
                b.iter(|| app.update())
            });
            drop(app);
        }
    }
    group.finish();

    let mut group = c.benchmark_group("flecs_create_delete_tree_hierarchy");
    for depth in [1, 10, 100, 1000] {
        for width in [1, 10, 100, 1000] {
            let mut app = App::new();
            app.add_plugins((HierarchyPlugin /* TransformPlugin */,))
                .add_systems(Update, move |commands: Commands| {
                    create_delete_tree_hierarchy(commands, depth, width)
                });
            group.bench_with_input(format!("d{}w{}", depth, width), &(depth, width), |b, _| {
                b.iter(|| app.update())
            });
            drop(app);
        }
    }
    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
