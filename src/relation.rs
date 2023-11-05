use crate::Var;
use core::any::TypeId;

/// Type ID of a relation.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RelationId(pub(crate) TypeId);

impl RelationId {
    #[allow(missing_docs)]
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

impl From<Hierarchy> for Var<RelationId> {
    fn from(_: Hierarchy) -> Self {
        Self::Val(RelationId(TypeId::of::<Hierarchy>()))
    }
}

/// Hack to ensure relation types are indeed ZSTs
pub trait ZstOrPanic: Sized {
    #[allow(missing_docs)]
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
/// struct O;
///
/// #[derive(Relation)]
/// #[aery(Recursive)]
/// struct R;
///
/// fn sys(world: &mut World) {
///     let [e0, e1, e2, e3, e4, e5, e6] = std::array::from_fn(|_| world.spawn_empty().id());
///
///     world.entity_mut(e1).set::<O>(e0).set::<R>(e0);
///
///     world.entity_mut(e2).set::<O>(e0);
///     world.entity_mut(e3).set::<O>(e1);
///
///     world.entity_mut(e4).set::<O>(e1).set::<R>(e1);
///
///     world.entity_mut(e5).set::<R>(e2);
///     world.entity_mut(e6).set::<R>(e2);
///
///     // Trigger cleanup
///     world.entity_mut(e0).checked_despawn();
///
///     for (entity, expected) in [
///         (e0, false),
///         (e1, false),
///         (e2, true),
///         (e3, true),
///         (e4, false),
///         (e5, true),
///         (e6, true)
///     ] {
///         assert_eq!(world.get_entity(entity).is_some(), expected)
///     }
/// }
///# use bevy::app::AppExit;
///#
///# fn exit_system(mut exit: EventWriter<AppExit>) {
///#     exit.send(AppExit);
///# }
///#
///# fn main() {
///#     App::new()
///#         .add_systems(Startup, (sys, exit_system).chain())
///#         .run();
///# }
/// ```
/// ## Before cleanup:
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
/// are "host" and "target" respectively. With both the host and target being entities.
/// Exclusive relations that face bottom up in hierarchies have
/// many favorable properties so these are the default.
///
/// Note that relations:
/// - Must be a [ZST](https://doc.rust-lang.org/nomicon/exotic-sizes.html#zero-sized-types-zsts).
/// This simply means that there can be no data on the edge.
/// A compile error will be produced if you try to use a relation that isn't one.
/// - Cannot be self referential. Ie. an entity cannot target itself with a relationship it hosts.
/// If this is ever attempted a warning will be logged & the relationship will not be set.
///
/// Aery only supports relations that are non-fragmenting. Ie. an entities archetype is not affected
/// by the targets of its relations. See [this article](https://ajmmertens.medium.com/building-an-ecs-2-archetypes-and-vectorization-fe21690805f9)
/// for more information. This isn't necessarily good or bad. Archetype fragmentation is a more
/// advanced topic but to keep it short and simple the archetype fragmentation is comparable to
/// `bevy_hierarchy` if it supported multiple hierarchy types.
/// ## Derive examples
/// ```
/// use aery::prelude::*;
///
/// // Simple derive with defaults:
/// // - Orphaning
/// // - Exclusive
/// // - Asymmetric
/// #[derive(Relation)]
/// struct R;
///
/// // Override edge exclusivity
/// #[derive(Relation)]
/// #[aery(Poly)]
/// struct Poly;
///
/// // Override edge symmetry
/// #[derive(Relation)]
/// #[aery(Symmetric)]
/// struct Symmetric;
///
/// // Override cleanup policy
/// #[derive(Relation)]
/// #[aery(Recursive)] // Available: Counted, Recursive, Total
/// struct Recursive;
///
/// // Override multiple properties
/// #[derive(Relation)]
/// #[aery(Poly, Symmetric, Counted)]
/// struct Multi;
/// ```
pub trait Relation: 'static + Sized + Send + Sync {
    /// How to clean up entities and relations when an entity with a relation is despawned
    /// or when a relation is unset.
    const CLEANUP_POLICY: CleanupPolicy = CleanupPolicy::Orphan;

    /// Whether or not an entity is allowed to host more than 1 of this relation type.
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

/// For compatibility with bevy_hierarchy.
/// **WARNING:**
/// - Hierarchy cleanup does not clean aery relations.
/// - Aery cleanup policies do not clean up hierarchy edges.
/// ## Query example
/// ```
/// use bevy::prelude::*;
/// use aery::prelude::*;
///
/// #[derive(Component)]
/// struct A {
///     // ..
/// }
///
/// #[derive(Component)]
/// struct B {
///     // ..
/// }
///
/// #[derive(Relation)]
/// struct R;
///
/// fn sys(
///     a_query: Query<&A>,
///     b_query: Query<(&B, Relations<(Hierarchy, R)>)>, // Can use alone or along side relations
///     roots: Query<Entity, (With<Children>, Without<Parent>)>
/// ) {
///     b_query.traverse::<Hierarchy>(roots.iter()).for_each(|b, edges| {
///         edges.join::<R>(&a_query).for_each(|a| {
///             // ..
///         });
///     })
/// }
/// ```
pub struct Hierarchy;
