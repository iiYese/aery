use crate::Var;

use bevy::{
    ecs::{
        component::Component,
        entity::Entity,
        query::{Or, With, WorldQuery},
        world::{EntityMut, EntityRef},
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

/// Filter to find roots of a relationship graph for quintessential traversal.
/// A root of any `R` is an entity that is the target of atleast 1 `R`
/// but does not itself target any other entities with `R`.
#[derive(WorldQuery)]
pub struct Root<R: Relation> {
    filter: With<RootMarker<R>>,
}

/// Filter to find any participants of a relationship.
#[derive(WorldQuery)]
pub struct Participates<R: Relation> {
    filter: Or<(With<Participant<R>>, With<RootMarker<R>>)>,
}

/// TODO: Mermaid
/// Supported cleanup patterns. When entities have multiple relations with different cleanup
/// policies each relation looks at the graph as if it were the only relation that existed.
/// In effect the summation of their cleanup is applied.
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
///     // Trigger cleanup
///     root.checked_despawn();
/// }
/// ```
/// ## After creation before cleanup:
#[cfg_attr(doc, aquamarine::aquamarine)]
/// ```mermaid
/// flowchart BT
/// E1 ---> |R| E0
/// E1 ---> |O| E0
///
/// E2 ---> |O| E0
///
/// E3 ---> |O| E1
///
/// E4 ---> |R| E1
/// E4 ---> |O| E1
///
/// E5 ---> |R| E2
///
/// E6 ---> |R| E2
/// ```
///
/// ## After cleanup:
#[cfg_attr(doc, aquamarine::aquamarine)]
/// ```mermaid
/// flowchart BT
/// E3
///
/// E5 ---> |R| E2
///
/// E6 ---> |R| E2
/// ```
#[derive(Clone, Copy)]
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

/// The relation trait. This is what controls the cleanup, exclusivity & symmetry of a relation.
/// Relations can be thought of as arrows. The terms Aery uses for the base and head of this arrow
/// are "host" and "target" respectively. With both the host and target being entities. Both the
/// host and target are "participants".
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

#[derive(Component, Default, Debug)]
pub(crate) struct Edges {
    pub hosts: [HashMap<RelationId, IndexSet<Entity>>; 4],
    pub targets: [HashMap<RelationId, IndexSet<Entity>>; 4],
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

#[derive(WorldQuery)]
pub struct EdgeWQ {
    pub(crate) edges: &'static Edges,
}

/// Trait to check what relations exist.
pub trait CheckRelations {
    /// Check if another entity is targeting this one via a relation.
    /// ```
    ///# use bevy::prelude::*;
    ///# use aery::{prelude::*, relation::EdgeWQItem};
    ///#
    ///# #[derive(Relation)]
    ///# struct R;
    ///#
    ///# fn foo(entity: EdgeWQItem<'_>, e: Entity) {
    /// // Check if entity is the target of `e` via `R`
    /// entity.has_host(R, e);
    ///
    /// // Check if entity is the target of any entity via `R`
    /// entity.has_host(R, Wc);
    ///
    /// // Check if entity is the target of any entity via any relationship
    /// entity.has_host(Wc, Wc);
    ///# }
    /// ```
    fn has_host(&self, relation: impl Into<Var<RelationId>>, host: impl Into<Var<Entity>>) -> bool;

    /// Check if entity is targeting another via a relation.
    /// ```
    ///# use bevy::prelude::*;
    ///# use aery::{prelude::*, relation::EdgeWQItem};
    ///#
    ///# #[derive(Relation)]
    ///# struct R;
    ///#
    ///# fn foo(entity: EdgeWQItem<'_>, e: Entity) {
    /// // Check if entity targets `e` via `R`
    /// entity.has_target(R, e);
    ///
    /// // Check if entity targets of any other entity via `R`
    /// entity.has_target(R, Wc);
    ///
    /// // Check if entity targets of any other entity via any relationship
    /// entity.has_target(Wc, Wc);
    ///# }
    /// ```
    fn has_target(
        &self,
        relation: impl Into<Var<RelationId>>,
        target: impl Into<Var<Entity>>,
    ) -> bool;
}

#[rustfmt::skip]
impl CheckRelations for Edges {
    fn has_host(&self, relation: impl Into<Var<RelationId>>, host: impl Into<Var<Entity>>) -> bool {
        match (relation.into(), host.into()) {
            (Var::Val(rel), Var::Val(host)) => self
                .hosts
                .iter()
                .any(|map| map.get(&rel).filter(|set| set.contains(&host)).is_some()),
            (Var::Val(rel), Var::Wc) => self
                .hosts
                .iter()
                .any(|map| map.get(&rel).is_some()),
            (Var::Wc, Var::Val(host)) => self
                .hosts
                .iter()
                .flat_map(|map| map.values())
                .any(|set| set.contains(&host)),
            (Var::Wc, Var::Wc) => self
                .hosts
                .iter()
                .flat_map(|map| map.keys())
                .next()
                .is_some(),
        }
    }

    fn has_target(&self,
        relation: impl Into<Var<RelationId>>,
        target: impl Into<Var<Entity>>,
    )
        -> bool
    {
        match (relation.into(), target.into()) {
            (Var::Val(rel), Var::Val(target)) => self
                .targets
                .iter()
                .any(|map| map.get(&rel).filter(|set| set.contains(&target)).is_some()),
            (Var::Val(rel), Var::Wc) => self
                .targets
                .iter()
                .any(|map| map.get(&rel).is_some()),
            (Var::Wc, Var::Val(target)) => self
                .targets
                .iter()
                .flat_map(|map| map.values())
                .any(|set| set.contains(&target)),
            (Var::Wc, Var::Wc) => self
                .targets
                .iter()
                .flat_map(|map| map.keys())
                .next()
                .is_some(),
        }
    }
}

impl CheckRelations for EdgeWQItem<'_> {
    fn has_host(&self, relation: impl Into<Var<RelationId>>, host: impl Into<Var<Entity>>) -> bool {
        self.edges.has_host(relation, host)
    }

    fn has_target(
        &self,
        relation: impl Into<Var<RelationId>>,
        target: impl Into<Var<Entity>>,
    ) -> bool {
        self.edges.has_target(relation, target)
    }
}

impl CheckRelations for EntityRef<'_> {
    fn has_host(
        &self,
        relation: impl Into<Var<RelationId>>,
        target: impl Into<Var<Entity>>,
    ) -> bool {
        self.get::<Edges>()
            .map_or(false, |edges| edges.has_host(relation, target))
    }

    fn has_target(
        &self,
        relation: impl Into<Var<RelationId>>,
        target: impl Into<Var<Entity>>,
    ) -> bool {
        self.get::<Edges>()
            .map_or(false, |edges| edges.has_target(relation, target))
    }
}

impl CheckRelations for EntityMut<'_> {
    fn has_host(
        &self,
        relation: impl Into<Var<RelationId>>,
        target: impl Into<Var<Entity>>,
    ) -> bool {
        self.get::<Edges>()
            .map_or(false, |edges| edges.has_host(relation, target))
    }

    fn has_target(
        &self,
        relation: impl Into<Var<RelationId>>,
        target: impl Into<Var<Entity>>,
    ) -> bool {
        self.get::<Edges>()
            .map_or(false, |edges| edges.has_target(relation, target))
    }
}
