use crate::Var;

use bevy::{
    ecs::{
        component::Component,
        entity::Entity,
        query::{Or, With, WorldQuery},
    },
    utils::HashMap,
};

use core::any::TypeId;
use indexmap::IndexSet;
use std::marker::PhantomData;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RelationId(TypeId);

impl RelationId {
    pub fn of<R: Relation>() -> Self {
        Self(TypeId::of::<R>())
    }
}

impl<R: Relation> From<R> for Var<RelationId> {
    fn from(_: R) -> Self {
        let _ = R::ZST_OR_PANIC;
        Self::Val(RelationId::of::<R>())
    }
}

#[derive(Component)]
pub(crate) struct RootMarker<R: Relation> {
    pub _phantom: PhantomData<R>,
}

#[derive(Component)]
pub(crate) struct Participant<R: Relation> {
    pub _phantom: PhantomData<R>,
}

/// Filter to find roots of a relationship graph.
#[derive(WorldQuery)]
pub struct Root<R: Relation> {
    filter: With<RootMarker<R>>,
}

/// Filter to find any participants of a relationship.
#[derive(WorldQuery)]
pub struct Participates<R: Relation> {
    filter: Or<(With<Participant<R>>, With<RootMarker<R>>)>,
}

/// Supported cleanup patterns. When entities have multiple relations with different cleanup
/// policies each relation looks at the graph as if it were the only relation that existed.
/// In effect the summation of their cleanup is applied.
/// Cleanup is triggered when the entities participating in a relationship change. Ie.
/// - When an entity is despawned
/// - When the relation is removed
/// # Example
/// ```
/// use bevy::prelude::*;
/// use aery::prelude::*;
///
/// #[derive(Relation)]
/// #[cleanup(policy = "Orphan")]
/// struct O;
///
/// #[derive(Relation)]
/// #[cleanup(policy = "Recursive")]
/// struct R;
///
/// fn sys(world: &mut World) {
///     let [e0, e1, e2, e3, e4, e5, e6] = std::array::from_fn(|_| world.spawn_empty().id());
///
///     world.set::<O>(e1, e0);
///     world.set::<O>(e2, e0);
///     world.set::<O>(e3, e1);
///     world.set::<O>(e4, e1);
///
///     world.set::<R>(e1, e0);
///     world.set::<R>(e4, e1);
///     world.set::<R>(e5, e2);
///     world.set::<R>(e6, e2);
///
///     // Results in:
///     //             0
///     //           // \
///     //          //   \
///     //         RO     O
///     //        //       \
///     //       //         \
///     //      1            2
///     //     / \\         / \
///     //    O   RO       R   R
///     //   /     \\     /     \
///     //  3       4    5       6
///
///     world.checked_despawn(e0);
///
///     // After cleanup:
///     //                   2
///     //                  / \
///     //      3          R   R
///     //                /     \
///     //               5       6
/// }
/// ```
#[derive(Clone, Copy)]
pub enum CleanupPolicy {
    /// Will do no further cleanup.
    Orphan,
    /// Counted relationships "count" the number of hosts they have. If it ever reaches zero they
    /// will delete themselves. This is effectively reference counting.
    Counted,
    /// When targets of recursively cleaning relations are deleted they also delete all their
    /// hosts. Unsetting a recursively cleaning relation is the same as despawning the host.
    Recursive,
    /// Total performs both counted and recursive cleanup.
    Total,
}

pub trait ZstOrPanic: Sized {
    const ZST_OR_PANIC: () = {
        // TODO: Make diagnostic friendlier when `std::any::type_name` becomes const
        // TODO: Use actual type level mechanism and remove hack when possible in stable
        if std::mem::size_of::<Self>() != 0 {
            panic!("Not a ZST")
        }
    };
}

impl<T> ZstOrPanic for T {}

/// The relation trait. This is what controls the cleanup policy and exclusivity of a relation.
pub trait Relation: 'static + Sized + Send + Sync {
    const CLEANUP_POLICY: CleanupPolicy = CleanupPolicy::Orphan;
    const EXCLUSIVE: bool = true;
    const SYMMETRIC: bool = false;
}

#[derive(Component, Default, Debug)]
pub(crate) struct Edges {
    pub hosts: [HashMap<RelationId, IndexSet<Entity>>; 4],
    pub targets: [HashMap<RelationId, IndexSet<Entity>>; 4],
}

#[derive(WorldQuery)]
pub struct EdgeWQ {
    pub(crate) edges: &'static Edges,
}

type EdgeIter<'a> = std::iter::Flatten<
    std::option::IntoIter<std::iter::Copied<indexmap::set::Iter<'a, bevy::prelude::Entity>>>,
>;

impl Edges {
    pub(crate) fn iter_hosts<R: Relation>(&self) -> EdgeIter<'_> {
        self.hosts[R::CLEANUP_POLICY as usize]
            .get(&RelationId::of::<R>())
            .map(|targets| targets.iter().copied())
            .into_iter()
            .flatten()
    }

    pub(crate) fn iter_targets<R: Relation>(&self) -> EdgeIter<'_> {
        self.targets[R::CLEANUP_POLICY as usize]
            .get(&RelationId::of::<R>())
            .map(|targets| targets.iter().copied())
            .into_iter()
            .flatten()
    }
}
