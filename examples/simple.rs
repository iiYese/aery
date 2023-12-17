use aery::prelude::*;
use bevy::{app::AppExit, prelude::*};

#[derive(Relation)]
struct ChildOf;

#[derive(Component)]
struct Name(&'static str);

fn setup(mut cmds: Commands) {
    cmds.spawn(Name("Alice")).scope::<ChildOf>(|chld| {
        chld.add(Name("Jack"))
            .scope::<ChildOf>(|chld| {
                chld.add(Name("Dave"));
            })
            .add(Name("Jill"))
            .scope::<ChildOf>(|chld| {
                chld.add(Name("Stephen")).add(Name("Sam"));
            });
    });

    cmds.spawn(Name("Loyd")).scope::<ChildOf>(|chld| {
        chld.add(Name("Anya"));
    });
}

fn display_children(tree: Query<(&Name, Relations<ChildOf>)>, roots: Query<Entity, Root<ChildOf>>) {
    tree.traverse::<ChildOf>(roots.iter())
        .track_self()
        .for_each(|Name(parent), _, Name(child), _| {
            println!("{} is the parent of {}", parent, child);
        });
}

fn exit(mut exit: EventWriter<AppExit>) {
    exit.send(AppExit);
}

fn main() {
    App::new()
        .add_plugins(Aery)
        .add_systems(Startup, setup)
        .add_systems(Update, (display_children, exit).chain())
        .run();
}
