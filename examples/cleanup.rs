use aery::prelude::*;
use bevy::{prelude::*, window::PrimaryWindow};

const WIN_SIZE: Vec2 = Vec2::new(800., 600.);

#[derive(Component)]
struct Pos(Vec2);

#[derive(Relation)]
// By default all relations are orhpaning.
// Supplying one of the other cleanup policies to the `aery` attribute will override that.
// Click on any of the nodes in this example to delete them.
// Change the policy and rerun the example to see the different behaviors.
#[aery(Recursive)]
struct ChildOf;

fn setup(mut cmds: Commands) {
    cmds.spawn(Camera2dBundle::default());

    cmds.spawn(Pos(Vec2::new(0.0, 150.0)))
        .scope::<ChildOf>(|chld| {
            chld.add(Pos(Vec2::new(-150., 0.)))
                .scope::<ChildOf>(|chld| {
                    chld.add(Pos(Vec2::new(-300., -150.)))
                        .add(Pos(Vec2::new(-100., -150.)));
                })
                .add(Pos(Vec2::new(150., 0.)))
                .scope::<ChildOf>(|chld| {
                    chld.add(Pos(Vec2::new(100., -150.)))
                        .add(Pos(Vec2::new(300., -150.)));
                });
        });
}

fn draw(mut gizmos: Gizmos, tree: Query<&Pos>) {
    for Pos(pos) in tree.iter() {
        gizmos.circle_2d(*pos, 40., Color::WHITE);
    }
}

fn input(
    mut cmds: Commands,
    mouse_buttons: Res<ButtonInput<MouseButton>>,
    windows: Query<&Window, With<PrimaryWindow>>,
    nodes: Query<(Entity, &Pos)>,
) {
    let Some(cursor_pos) = windows
        .single()
        .cursor_position()
        .filter(|_| mouse_buttons.just_pressed(MouseButton::Left))
        .map(|pos| pos - WIN_SIZE / 2.)
        .map(|pos| Vec2::new(pos.x, -pos.y))
    else {
        return;
    };

    if let Some((e, _)) = nodes
        .iter()
        .find(|(_, Pos(pos))| (cursor_pos - *pos).length() < 40.)
    {
        cmds.entity(e).despawn();
    }
}

fn main() {
    App::new()
        .add_plugins(DefaultPlugins.set(WindowPlugin {
            primary_window: Some(Window {
                resolution: WIN_SIZE.into(),
                ..default()
            }),
            ..default()
        }))
        .add_systems(Startup, setup)
        .add_systems(Update, (input, draw))
        .run();
}
