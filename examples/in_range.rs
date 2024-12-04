use aery::prelude::*;
use bevy::log;
use bevy::prelude::*;
use std::fmt::Display;

#[derive(Component)]
struct MovingEntity {
    direction: f32, // Movement direction along the x-axis, positive or negative.
}

#[derive(Component)]
struct Name(&'static str);

impl Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0)
    }
}

// Define a relationship where multiple entities can be linked non-exclusively.
// Symmetry is not set in this example, it may arise from the implementation,
// when we set Poly relation from both sides.
// But adding the aery `Symmetric` attribute to the struct would enforce it, and MUST show same results.
#[derive(Relation, Component)]
#[aery(Poly)]
struct InRange;

fn setup(mut commands: Commands) {
    // Spawning two moving entities with initial positions and directions.
    commands.spawn((
        Name("Alice"),
        MovingEntity { direction: 1.0 },
        Transform::from_xyz(-150.0, 0.0, 0.0),
        GlobalTransform::default(),
    ));
    commands.spawn((
        Name("Bob"),
        MovingEntity { direction: -1.0 },
        Transform::from_xyz(150.0, 0.0, 0.0),
        GlobalTransform::default(),
    ));
}

fn move_entities(mut query: Query<(&mut Transform, &mut MovingEntity)>, time: Res<Time>) {
    for (mut transform, mut moving_entity) in query.iter_mut() {
        // Update the position based on direction and elapsed time.
        transform.translation.x += moving_entity.direction * time.delta_secs() * 50.0;

        // Reverse direction if the entity moves beyond the bounds.
        if transform.translation.x > 200.0 || transform.translation.x < -200.0 {
            moving_entity.direction *= -1.0;
        }
    }
}

fn set_relations(
    mut commands: Commands,
    time: Res<Time>,
    mut last_time: Local<f32>,
    in_range_abstains: Query<(Entity, &Transform), (With<MovingEntity>, Abstains<InRange>)>,
    query_all: Query<(Entity, &Transform), With<MovingEntity>>,
) {
    if time.elapsed_secs() - *last_time >= 0.1 {
        *last_time = time.elapsed_secs();
        for (entity_a, pos_a) in in_range_abstains.iter() {
            for (entity_b, pos_b) in query_all.iter() {
                if entity_a != entity_b {
                    // Add the "InRange" relation if the distance is within the threshold.
                    let distance = pos_a.translation.distance(pos_b.translation);
                    if distance <= 100.0 {
                        commands.entity(entity_a).set::<InRange>(entity_b);
                    }
                }
            }
        }
    }
}

fn unset_relations(
    mut commands: Commands,
    time: Res<Time>,
    mut last_time: Local<f32>,
    in_range_participates: Query<
        (Entity, &Transform, Relations<InRange>),
        (With<MovingEntity>, Participates<InRange>),
    >,
    moving_entities: Query<(Entity, &Transform), With<MovingEntity>>,
) {
    if time.elapsed_secs() - *last_time >= 0.1 {
        *last_time = time.elapsed_secs();
        for (entity_a, pos_a, relations) in in_range_participates.iter() {
            let edges_iter = relations.join::<InRange>(&moving_entities);
            edges_iter.for_each(|(entity_b, _): (Entity, &Transform)| {
                if entity_a != entity_b {
                    // Remove the relation if the entities are no longer in range.
                    if let Ok((_, pos_b)) = moving_entities.get(entity_b) {
                        let distance = pos_a.translation.distance(pos_b.translation);
                        if distance > 100.0 {
                            commands.entity(entity_a).unset::<InRange>(entity_b);
                        }
                    } else {
                        // Unset in case the target entity no longer exists.
                        commands.entity(entity_a).unset::<InRange>(entity_b);
                    }
                }
            });
        }
    }
}

fn relation_set(set: Trigger<SetEvent<InRange>>) {
    // Do something when a new "InRange" relationship is established.
    let first = set.entity();
    let second = set.event().target;
    log::info!("{} is now in range of {}", first, second);
}

fn relation_remove(unset: Trigger<UnsetEvent<InRange>>) {
    // Do something when an "InRange" relationship is removed.
    let first = unset.entity();
    let second = unset.event().target;
    log::info!("{} is no longer in range of {}", first, second);
}

pub struct InRangePlugin;
impl Plugin for InRangePlugin {
    fn build(&self, app: &mut App) {
        app.register_relation::<InRange>();
        app.add_systems(Update, (set_relations, unset_relations));
        app.add_observer(relation_set);
        app.add_observer(relation_remove);
    }
}

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_systems(Startup, setup)
        .add_plugins(InRangePlugin)
        .add_systems(Update, move_entities)
        .run();
}
