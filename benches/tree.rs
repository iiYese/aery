use aery::{commands::CheckedDespawn, prelude::*};
use bevy::{
    ecs::{schedule::ScheduleLabel, world::EntityMut},
    prelude::*,
};
use criterion::{criterion_group, criterion_main, BatchSize, Criterion};
use std::cell::RefCell;

#[derive(ScheduleLabel, Clone, Debug, PartialEq, Eq, Hash)]
pub struct TestSetup;

#[derive(Component)]
struct Head;

#[derive(Component)]
struct Mid;

#[derive(Component)]
struct Tail;

#[derive(Relation)]
#[cleanup(policy = "Recursive")]
struct ChildOf;

fn create_tree_aery(mut commands: Commands, levels: &[(usize, usize)]) {
    let levels = levels.to_owned();
    commands.add(move |world: &mut World| {
        let Some((a, b)) = levels.first() else {
            return;
        };
        for i in 0..(a + b) {
            let ent = world.spawn((
                Head,
                TransformBundle::from_transform(Transform::from_xyz((i as f32) * 1.0, 0.0, 0.0)),
            ));
            if i < *a {
                create_tree_aery_rec(ent, &levels[1..]);
            }
        }
    });
}

fn create_tree_aery_rec<'a>(mut parent: EntityMut<'a>, levels: &[(usize, usize)]) {
    let Some((a, b)) = levels.first() else {
        return;
    };
    for i in 0..(a + b) {
        parent = parent
            .scope::<ChildOf>(|_, mut ent| {
                ent.insert(TransformBundle::from_transform(Transform::from_xyz(
                    0.0, 0.0, 0.0,
                )));
                if i < *a {
                    create_tree_aery_rec(ent, &levels[1..]);
                }
            })
            .unwrap();
    }
}

fn create_tree_hierarchy(mut commands: Commands, levels: &[(usize, usize)]) {
    let Some((a, b)) = levels.first() else {
        return;
    };
    for i in 0..(a + b) {
        commands
            .spawn((
                Head,
                TransformBundle::from_transform(Transform::from_xyz((i as f32) * 1.0, 0.0, 0.0)),
            ))
            .with_children(|builder| {
                if i < *a {
                    create_tree_hierarchy_rec(builder, &levels[1..]);
                }
            });
    }
}

fn create_tree_hierarchy_rec(builder: &mut ChildBuilder, levels: &[(usize, usize)]) {
    let Some((a, b)) = levels.first() else {
        return;
    };
    for i in 0..(a + b) {
        builder
            .spawn(TransformBundle::from_transform(Transform::from_xyz(
                0.0, 0.0, 0.0,
            )))
            .with_children(|builder| {
                if i < *a {
                    create_tree_hierarchy_rec(builder, &levels[1..]);
                }
            });
    }
}

#[allow(dead_code)]
fn remove_all(query: Query<Entity>, mut commands: Commands) {
    for entity in query.iter() {
        commands.entity(entity).despawn();
    }
}

fn remove_heads_hierarchy(query: Query<Entity, With<Head>>, mut commands: Commands) {
    for entity in query.iter() {
        commands.entity(entity).despawn_recursive();
    }
}

fn remove_heads_aery(query: Query<Entity, With<Head>>, mut commands: Commands) {
    for entity in query.iter() {
        commands.add(CheckedDespawn { entity });
    }
}

fn rotate_boxes(mut query: Query<&mut Transform>) {
    for mut foo in query.iter_mut() {
        foo.rotate_x(0.01);
    }
}

fn update_global_transform_aery(
    mut query: ParamSet<(
        Query<(&Transform, &mut GlobalTransform), Root<ChildOf>>,
        Query<((&Transform, &mut GlobalTransform), Relations<ChildOf>)>,
    )>,
    roots: Query<Entity, Root<ChildOf>>,
) {
    for (root_t, mut root_gt) in query.p0().iter_mut() {
        *root_gt = GlobalTransform::from(*root_t);
    }
    query
        .p1()
        .ops_mut()
        .traverse::<ChildOf>(roots.iter())
        .for_each(|(_parent_t, parent_gt), (child_t, mut child_gt)| {
            *child_gt = parent_gt.mul_transform(*child_t);
            //println!("{:?} {:?} {:?} {:?}", parent_t, parent_gt, child_t, child_gt);
        });
}

pub fn criterion_benchmark(c: &mut Criterion) {
    let scenarios = [
        ("flat", &[(0, 65536)][..]),
        ("small", &[(4096, 0), (4, 0), (4, 0)][..]),
        ("wide", &[(128, 0), (512, 0)][..]),
        ("deep", &[(1, 16); 4096][..]),
    ];

    let mut group = c.benchmark_group("tree_transform_aery");
    for (name, levels) in scenarios {
        let mut app = App::new();
        app.add_plugins(Aery)
            .add_systems(Startup, |commands: Commands| {
                create_tree_aery(commands, levels)
            })
            .add_systems(Update, rotate_boxes)
            .add_systems(PostUpdate, update_global_transform_aery);
        app.update();
        group.bench_with_input(name, &(), |b, _| b.iter(|| app.update()));
        drop(app);
    }
    group.finish();

    let mut group = c.benchmark_group("tree_transform_hierarchy");
    for (name, levels) in scenarios {
        let mut app = App::new();
        app.add_plugins((HierarchyPlugin, TransformPlugin))
            .add_systems(Startup, |commands: Commands| {
                create_tree_hierarchy(commands, levels)
            })
            .add_systems(Update, rotate_boxes);
        group.bench_with_input(name, &(), |b, _| b.iter(|| app.update()));
        drop(app);
    }
    group.finish();

    let mut group = c.benchmark_group("tree_spawn_aery");
    for (name, levels) in scenarios {
        let mut app = App::new();
        app.add_plugins(Aery)
            .add_systems(TestSetup, remove_heads_aery)
            .add_systems(Update, |commands: Commands| {
                create_tree_aery(commands, levels)
            });
        let appref = RefCell::new(app);
        group.bench_with_input(name, &(), |b, _| {
            b.iter_batched(
                || appref.borrow_mut().world.run_schedule(TestSetup),
                |_| appref.borrow_mut().update(),
                BatchSize::PerIteration,
            )
        });
        drop(appref);
    }
    group.finish();

    let mut group = c.benchmark_group("tree_spawn_hierarchy");
    for (name, levels) in scenarios {
        let mut app = App::new();
        app.add_plugins((HierarchyPlugin /* TransformPlugin */,))
            .add_systems(TestSetup, remove_heads_hierarchy)
            .add_systems(Update, |commands: Commands| {
                create_tree_hierarchy(commands, levels)
            });
        let appref = RefCell::new(app);
        group.bench_with_input(name, &(), |b, _| {
            b.iter_batched(
                || appref.borrow_mut().world.run_schedule(TestSetup),
                |_| appref.borrow_mut().update(),
                BatchSize::PerIteration,
            )
        });
        drop(appref);
    }
    group.finish();

    let mut group = c.benchmark_group("tree_despawn_aery");
    for (name, levels) in scenarios {
        let mut app = App::new();
        app.add_plugins(Aery)
            .add_systems(TestSetup, |commands: Commands| {
                create_tree_aery(commands, levels)
            })
            .add_systems(Update, remove_heads_aery);
        let appref = RefCell::new(app);
        group.bench_with_input(name, &(), |b, _| {
            b.iter_batched(
                || appref.borrow_mut().world.run_schedule(TestSetup),
                |_| appref.borrow_mut().update(),
                BatchSize::PerIteration,
            )
        });
        drop(appref);
    }
    group.finish();

    let mut group = c.benchmark_group("tree_despawn_hierarchy");
    for (name, levels) in scenarios {
        let mut app = App::new();
        app.add_plugins((HierarchyPlugin /* TransformPlugin */,))
            .add_systems(TestSetup, |commands: Commands| {
                create_tree_hierarchy(commands, levels)
            })
            .add_systems(Update, remove_heads_hierarchy);
        let appref = RefCell::new(app);
        group.bench_with_input(name, &(), |b, _| {
            b.iter_batched(
                || appref.borrow_mut().world.run_schedule(TestSetup),
                |_| appref.borrow_mut().update(),
                BatchSize::PerIteration,
            )
        });
        drop(appref);
    }
    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
