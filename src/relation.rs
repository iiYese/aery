use crate::{
    tuple_traits::{PadMax, RelationSet},
    Var,
};
use bevy::ecs::query::{ReadOnlyWorldQuery, WorldQuery};
use core::any::TypeId;

// TODO 0.12 impl for Hierarchy
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RelationId(pub(crate) TypeId);

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

/// Hack to ensure relation types are indeed ZSTs
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

#[cfg_attr(doc, aquamarine::aquamarine)]
/// Supported cleanup patterns. When entities have multiple relations with different cleanup
/// policies each relation looks at the graph as if it were the only relation that existed.
/// In effect the summation of their cleanup is applied.
/// ## Illustration
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
/// fn sys(wrld: &mut World) {
///     // Creation
///     let root = wrld
///         .spawn_empty()
///         .scope::<O>(|parent, ent1| {
///             ent1.set::<R>(parent)
///                 .scope::<O>(|parent, ent3| {})
///                 .scope::<O>(|parent, ent4| { ent4.set::<R>(parent); });
///         })
///         .scope::<O>(|_, ent2| {
///             ent2.scope::<R>(|_, ent5| {})
///                 .scope::<R>(|_, ent6| {});
///         });
///
///     // Trigger cleanup
///     root.checked_despawn();
/// }
/// ```
/// ## After creation before cleanup:
/// ```mermaid
/// flowchart BT
/// E1 --R--> E0
/// E1 --O--> E0
///
/// E2 --O--> E0
///
/// E3 --O--> E1
///
/// E4 --R--> E1
/// E4 --O--> E1
///
/// E5 --R--> E2
///
/// E6 --R--> E2
/// ```
///
/// ## After cleanup:
/// ```mermaid
/// flowchart BT
/// E3
///
/// E5 --R--> E2
///
/// E6 --R--> E2
/// ```
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum CleanupPolicy {
    /// Will do no further cleanup.
    Orphan,

    /// Entities that are the target of counted relationships *count* the number of hosts they have.
    /// If it ever reaches zero they will delete themselves. This is effectively reference counting.
    Counted,

    /// When entities that are the target of recursive relationships are despawned they also
    /// *recursively* despawn their hosts. Unsetting **does not** trigger recursive cleanup.
    Recursive,

    /// Total performs both counted and recursive cleanup.
    Total,
}

/// The relation trait. This is what controls the cleanup, exclusivity & symmetry of a relation.
/// Relations can be thought of as arrows. The terms Aery uses for the base and head of this arrow
/// are "host" and "target" respectively. With both the host and target being entities. Both the
/// host and target are "participants". Exclusive relations that face bottom up in hierarchies have
/// many favorable properties so these are the default.
///
/// Note that relations **must** be a [ZST](https://doc.rust-lang.org/nomicon/exotic-sizes.html#zero-sized-types-zsts).
/// A compile error will be produced if you try to use a relation that isn't one.
///
/// Aery only supports relations that are non-fragmenting. Ie. an entities archetype is not affected
/// by the targets of its relations. See [this article](https://ajmmertens.medium.com/building-an-ecs-2-archetypes-and-vectorization-fe21690805f9)
/// for more information. This isn't necessarily good or bad. There are various tradeoffs but it
/// would be overwhelming to explain them all at once. To keep it quick the archetype fragmentation
/// is comparable to `bevy_hierarchy` if it supported multiple hierarchy types.
pub trait Relation: 'static + Sized + Send + Sync {
    /// How to clean up entities and relations when an entity with a relation is despawned
    /// or when a relation is unset.
    const CLEANUP_POLICY: CleanupPolicy = CleanupPolicy::Orphan;

    /// Whether or not an entity is allowed to have more than 1 of this relation type.
    /// Entities can still be targeted multiple times by different entities with this relation.
    /// Entities cannot however host more than 1 of this relation at a time.
    /// Setting an exclusive relation that is already set will unset the existing relation.
    const EXCLUSIVE: bool = true;

    /// Whether or not a relation is symmetric. Ie:
    /// - When `e0 -R-> e1`
    /// - Then `e0 <-R- e1`
    ///
    /// For example it would make sense for a `MarriedTo` relation to be symmetric.
    const SYMMETRIC: bool = false;
}

// TODO: Enable for 0.12
// For hierarchy compatibility
//pub struct Hierarchy;
