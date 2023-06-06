use bevy::{
    ecs::{
        component::Component,
        entity::Entity,
        query::{Or, With, WorldQuery},
        system::{Command, Commands, Resource},
        world::World,
    },
    utils::{HashMap, HashSet},
};

use core::any::TypeId;
use indexmap::IndexSet;
use std::{collections::VecDeque, marker::PhantomData};

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
/// struct O;
///
/// impl Relation for O {
///     const CLEANUP_POLICY: CleanupPolicy = CleanupPolicy::Orphan;
/// }
///
/// struct R;
///
/// impl Relation for R {
///     const CLEANUP_POLICY: CleanupPolicy = CleanupPolicy::Recursive;
/// }
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
    /// Counted relationships "count" the number of fosters they have. If it ever reaches zero they
    /// will delete themselves. This is effectively reference counting.
    Counted,
    /// When targets of recursively cleaning relations are deleted they also delete all their
    /// fosters. Unsetting a recursively cleaning relation is the same as despawning the foster.
    Recursive,
    /// Total performs both counted and recursive cleanup.
    Total,
}

/// The relation trait. This is what controls the cleanup policy and exclusivity of a relation.
pub trait Relation: 'static + Send + Sync {
    const CLEANUP_POLICY: CleanupPolicy = CleanupPolicy::Orphan;
    const EXCLUSIVE: bool = true;
}

#[derive(Component, Default, Debug)]
pub(crate) struct Edges {
    pub fosters: [HashMap<TypeId, IndexSet<Entity>>; 4],
    pub targets: [HashMap<TypeId, IndexSet<Entity>>; 4],
}

#[derive(WorldQuery)]
pub struct EdgeWQ {
    pub(crate) edges: &'static Edges,
}

type EdgeIter<'a> = std::iter::Flatten<
    std::option::IntoIter<std::iter::Copied<indexmap::set::Iter<'a, bevy::prelude::Entity>>>,
>;

impl Edges {
    pub(crate) fn iter_fosters<R: Relation>(&self) -> EdgeIter<'_> {
        self.fosters[R::CLEANUP_POLICY as usize]
            .get(&TypeId::of::<R>())
            .map(|targets| targets.iter().copied())
            .into_iter()
            .flatten()
    }

    pub(crate) fn iter_targets<R: Relation>(&self) -> EdgeIter<'_> {
        self.targets[R::CLEANUP_POLICY as usize]
            .get(&TypeId::of::<R>())
            .map(|targets| targets.iter().copied())
            .into_iter()
            .flatten()
    }
}

pub(crate) fn refragment<R: Relation>(world: &mut World, entity: Entity) {
    let Some(mut edges) = world.get_mut::<Edges>(entity) else {
        return
    };

    let (has_fosters, has_targets) = (
        edges.fosters[R::CLEANUP_POLICY as usize]
            .get(&TypeId::of::<R>())
            .map(|fosters| !fosters.is_empty())
            .unwrap_or(false),
        edges.targets[R::CLEANUP_POLICY as usize]
            .get(&TypeId::of::<R>())
            .map(|targets| !targets.is_empty())
            .unwrap_or(false),
    );

    if !has_fosters {
        edges.fosters[R::CLEANUP_POLICY as usize].remove(&TypeId::of::<R>());
    }

    if !has_targets {
        edges.targets[R::CLEANUP_POLICY as usize].remove(&TypeId::of::<R>());
    }

    if edges
        .targets
        .iter()
        .chain(edges.fosters.iter())
        .all(HashMap::is_empty)
    {
        world.entity_mut(entity).remove::<Edges>();
    }

    match (has_fosters, has_targets) {
        (_, true) => {
            world
                .entity_mut(entity)
                .remove::<RootMarker<R>>()
                .insert(Participant::<R> {
                    _phantom: PhantomData,
                });
        }
        (true, false) => {
            world
                .entity_mut(entity)
                .remove::<Participant<R>>()
                .insert(RootMarker::<R> {
                    _phantom: PhantomData,
                });
        }
        (false, false) => {
            world
                .entity_mut(entity)
                .remove::<(Participant<R>, RootMarker<R>)>();
        }
    }
}

#[derive(Resource, Default)]
pub(crate) struct RefragmentHooks {
    hooks: HashMap<TypeId, fn(&mut World, Entity)>,
}

/// Command to set relationships between entities. Use convenience function [`set`].
pub struct Set<R>
where
    R: Relation,
{
    pub foster: Entity,
    pub target: Entity,
    pub _phantom: PhantomData<R>,
}

impl<R> Command for Set<R>
where
    R: Relation,
{
    fn write(self, world: &mut World) {
        if self.foster == self.target {
            // TODO: Logging
            return;
        }

        if world.get_entity(self.target).is_none() {
            // TODO: Logging
            return;
        }

        if world.get_entity(self.foster).is_none() {
            // TODO: Logging
            return;
        }

        world
            .resource_mut::<RefragmentHooks>()
            .hooks
            .insert(TypeId::of::<R>(), refragment::<R>);

        let mut target_edges = world
            .get_mut::<Edges>(self.target)
            .map(|mut edges| std::mem::take(&mut *edges))
            .unwrap_or_default();

        target_edges.fosters[R::CLEANUP_POLICY as usize]
            .entry(TypeId::of::<R>())
            .or_default()
            .insert(self.foster);

        if !target_edges.targets[R::CLEANUP_POLICY as usize].contains_key(&TypeId::of::<R>()) {
            world.entity_mut(self.target).insert(RootMarker::<R> {
                _phantom: PhantomData,
            });
        }

        world.entity_mut(self.target).insert(target_edges);

        let mut foster_edges = world
            .entity_mut(self.foster)
            .remove::<RootMarker<R>>()
            .get_mut::<Edges>()
            .map(|mut edges| std::mem::take(&mut *edges))
            .unwrap_or_default();

        let old = foster_edges.targets[R::CLEANUP_POLICY as usize]
            .get(&TypeId::of::<R>())
            .and_then(|targets| targets.first())
            .copied();

        foster_edges.targets[R::CLEANUP_POLICY as usize]
            .entry(TypeId::of::<R>())
            .or_default()
            .insert(self.target);

        world.entity_mut(self.foster).insert((
            foster_edges,
            Participant::<R> {
                _phantom: PhantomData,
            },
        ));

        if let Some(old) = old.filter(|old| R::EXCLUSIVE && self.target != *old) {
            Command::write(
                Unset::<R> {
                    foster: self.foster,
                    target: old,
                    _phantom: PhantomData,
                },
                world,
            );
        }
    }
}

/// Convenience trait to sugar adding relations.
pub trait SetExt {
    fn set<R: Relation>(&mut self, foster: Entity, target: Entity);
}

impl SetExt for Commands<'_, '_> {
    fn set<R: Relation>(&mut self, foster: Entity, target: Entity) {
        self.add(Set::<R> {
            foster,
            target,
            _phantom: PhantomData,
        });
    }
}

impl SetExt for World {
    fn set<R: Relation>(&mut self, foster: Entity, target: Entity) {
        Command::write(
            Set::<R> {
                foster,
                target,
                _phantom: PhantomData,
            },
            self,
        );
    }
}

/// Command to remove relationships between entities.
pub struct Unset<R>
where
    R: Relation,
{
    pub foster: Entity,
    pub target: Entity,
    pub _phantom: PhantomData<R>,
}

impl<R: Relation> Command for Unset<R> {
    fn write(self, world: &mut World) {
        Command::write(
            UnsetErased {
                foster: self.foster,
                target: self.target,
                typeid: TypeId::of::<R>(),
                policy: R::CLEANUP_POLICY,
            },
            world,
        );
    }
}

struct UnsetErased {
    foster: Entity,
    target: Entity,
    typeid: TypeId,
    policy: CleanupPolicy,
}

impl Command for UnsetErased {
    fn write(self, world: &mut World) {
        let Some(refragment) = world
            .resource::<RefragmentHooks>()
            .hooks
            .get(&self.typeid)
            .copied()
        else {
            return
        };

        let Some(mut foster_edges) = world
            .get_mut::<Edges>(self.foster)
            .map(|mut edges| std::mem::take(&mut *edges))
        else {
            return
        };

        let Some(mut target_edges) = world
            .get_mut::<Edges>(self.target)
            .map(|mut edges| std::mem::take(&mut *edges))
        else {
            world.entity_mut(self.foster).insert(foster_edges);
            return
        };

        foster_edges.targets[self.policy as usize]
            .entry(self.typeid)
            .and_modify(|fosters| {
                fosters.remove(&self.target);
            });

        target_edges.fosters[self.policy as usize]
            .entry(self.typeid)
            .and_modify(|fosters| {
                fosters.remove(&self.foster);
            });

        let target_orphaned = target_edges.fosters[self.policy as usize]
            .get(&self.typeid)
            .map_or(false, IndexSet::is_empty);

        world.entity_mut(self.foster).insert(foster_edges);
        world.entity_mut(self.target).insert(target_edges);

        match self.policy {
            CleanupPolicy::Orphan => {
                refragment(world, self.foster);
                refragment(world, self.target);
            }
            CleanupPolicy::Recursive => {
                Command::write(
                    CheckedDespawn {
                        entity: self.foster,
                    },
                    world,
                );
                refragment(world, self.target);
            }
            CleanupPolicy::Counted => {
                if target_orphaned {
                    Command::write(
                        CheckedDespawn {
                            entity: self.target,
                        },
                        world,
                    );
                }
                refragment(world, self.foster);
            }
            CleanupPolicy::Total => {
                Command::write(
                    CheckedDespawn {
                        entity: self.foster,
                    },
                    world,
                );
                if target_orphaned {
                    Command::write(
                        CheckedDespawn {
                            entity: self.target,
                        },
                        world,
                    );
                }
            }
        }
    }
}

/// Convenience trait to sugar removing relations.
pub trait UnsetExt {
    fn unset<R: Relation>(&mut self, foster: Entity, target: Entity);
}

impl UnsetExt for Commands<'_, '_> {
    fn unset<R: Relation>(&mut self, foster: Entity, target: Entity) {
        self.add(Unset::<R> {
            foster,
            target,
            _phantom: PhantomData,
        });
    }
}

impl UnsetExt for World {
    fn unset<R: Relation>(&mut self, foster: Entity, target: Entity) {
        Command::write(
            Unset::<R> {
                foster,
                target,
                _phantom: PhantomData,
            },
            self,
        );
    }
}

/// Command to despawn entities with rleations. Despawning via any other method can lead to
/// dangling which will not produce correct behavior!
pub struct CheckedDespawn {
    pub entity: Entity,
}

impl Command for CheckedDespawn {
    fn write(self, world: &mut World) {
        let mut to_refrag = HashMap::<TypeId, HashSet<Entity>>::new();
        let mut to_despawn = HashSet::<Entity>::from([self.entity]);
        let mut queue = VecDeque::from([self.entity]);

        let mut graph = world.query::<&mut Edges>();

        while let Some(curr) = queue.pop_front() {
            let Ok(edges) = graph
                .get_mut(world, curr)
                .map(|mut edges| std::mem::take(&mut *edges))
            else {
                continue
            };

            // Total relations
            for (typeid, target) in edges.targets[CleanupPolicy::Total as usize]
                .iter()
                .flat_map(|(typeid, targets)| targets.iter().map(move |target| (typeid, target)))
            {
                let Ok(mut target_edges) = graph.get_mut(world, *target) else {
                    continue
                };

                let Some(target_fosters) = target_edges
                    .fosters[CleanupPolicy::Total as usize]
                    .get_mut(typeid)
                else {
                    continue
                };

                target_fosters.remove(&curr);

                if target_fosters.is_empty() {
                    queue.push_back(*target);
                    to_despawn.insert(*target);
                }
            }

            for (typeid, foster) in edges.fosters[CleanupPolicy::Total as usize]
                .iter()
                .flat_map(|(typeid, fosters)| fosters.iter().map(move |foster| (typeid, foster)))
            {
                let Ok(mut foster_edges) = graph.get_mut(world, *foster) else {
                    continue
                };

                foster_edges.targets[CleanupPolicy::Total as usize]
                    .entry(*typeid)
                    .and_modify(|bucket| {
                        bucket.remove(&curr);
                    });

                queue.push_back(*foster);
                to_despawn.insert(*foster);
            }

            // Recursive relations
            for (typeid, target) in edges.targets[CleanupPolicy::Recursive as usize]
                .iter()
                .flat_map(|(typeid, targets)| targets.iter().map(move |target| (typeid, target)))
            {
                let Ok(mut target_edges) = graph.get_mut(world, *target) else {
                    continue
                };

                let Some(target_fosters) = target_edges
                    .fosters[CleanupPolicy::Recursive as usize]
                    .get_mut(typeid)
                else {
                    continue
                };

                target_fosters.remove(&curr);
                to_refrag.entry(*typeid).or_default().insert(*target);
            }

            for (typeid, foster) in edges.fosters[CleanupPolicy::Recursive as usize]
                .iter()
                .flat_map(|(typeid, fosters)| fosters.iter().map(move |foster| (typeid, foster)))
            {
                let Ok(mut foster_edges) = graph.get_mut(world, *foster) else {
                    continue
                };

                foster_edges.targets[CleanupPolicy::Recursive as usize]
                    .entry(*typeid)
                    .and_modify(|bucket| {
                        bucket.remove(&curr);
                    });

                queue.push_back(*foster);
                to_despawn.insert(*foster);
            }

            // Counted relations
            for (typeid, target) in edges.targets[CleanupPolicy::Counted as usize]
                .iter()
                .flat_map(|(typeid, targets)| targets.iter().map(move |target| (typeid, target)))
            {
                let Ok(mut target_edges) = graph.get_mut(world, *target) else {
                    continue
                };

                let Some(target_fosters) = target_edges
                    .fosters[CleanupPolicy::Counted as usize]
                    .get_mut(typeid)
                else {
                    continue
                };

                target_fosters.remove(&curr);

                if target_fosters.is_empty() {
                    queue.push_back(*target);
                    to_despawn.insert(*target);
                }
            }

            for (typeid, foster) in edges.fosters[CleanupPolicy::Counted as usize]
                .iter()
                .flat_map(|(typeid, fosters)| fosters.iter().map(move |foster| (typeid, foster)))
            {
                let Ok(mut foster_edges) = graph.get_mut(world, *foster) else {
                    continue
                };

                foster_edges.targets[CleanupPolicy::Counted as usize]
                    .entry(*typeid)
                    .and_modify(|bucket| {
                        bucket.remove(&curr);
                    });

                to_refrag.entry(*typeid).or_default().insert(*foster);
            }

            // Orphaning relations
            for (typeid, target) in edges.targets[CleanupPolicy::Orphan as usize]
                .iter()
                .flat_map(|(typeid, targets)| targets.iter().map(move |target| (typeid, target)))
            {
                let Ok(mut target_edges) = graph.get_mut(world, *target) else {
                    continue
                };

                target_edges.fosters[CleanupPolicy::Orphan as usize]
                    .entry(*typeid)
                    .and_modify(|bucket| {
                        bucket.remove(&curr);
                    });

                to_refrag.entry(*typeid).or_default().insert(*target);
            }

            for (typeid, foster) in edges.fosters[CleanupPolicy::Orphan as usize]
                .iter()
                .flat_map(|(typeid, fosters)| fosters.iter().map(move |foster| (typeid, foster)))
            {
                let Ok(mut foster_edges) = graph.get_mut(world, *foster) else {
                    continue
                };

                foster_edges.targets[CleanupPolicy::Orphan as usize]
                    .entry(*typeid)
                    .and_modify(|bucket| {
                        bucket.remove(&curr);
                    });

                to_refrag.entry(*typeid).or_default().insert(*foster);
            }
        }

        for entity in to_despawn {
            world.despawn(entity);
        }

        for (typeid, entities) in to_refrag {
            let refrag = world
                .resource::<RefragmentHooks>()
                .hooks
                .get(&typeid)
                .copied()
                .unwrap();

            for entity in entities {
                refrag(world, entity);
            }
        }
    }
}

/// Convenience trait to sugar despawning entities while checking for relations.
pub trait CheckedDespawnExt {
    fn checked_despawn(&mut self, entity: Entity);
}

impl CheckedDespawnExt for Commands<'_, '_> {
    fn checked_despawn(&mut self, entity: Entity) {
        self.add(CheckedDespawn { entity });
    }
}

impl CheckedDespawnExt for World {
    fn checked_despawn(&mut self, entity: Entity) {
        Command::write(CheckedDespawn { entity }, self);
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        prelude::*,
        relation::{Edges, Participant, RefragmentHooks, RootMarker},
    };
    use bevy::prelude::*;
    use core::any::TypeId;
    use std::array::from_fn;

    fn has_edges(world: &World, entity: Entity) -> bool {
        world.get::<Edges>(entity).is_some()
    }

    fn is_root<R: Relation>(world: &World, entity: Entity) -> bool {
        world.get::<RootMarker<R>>(entity).is_some()
    }

    fn is_participant<R: Relation>(world: &World, entity: Entity) -> bool {
        world.get::<Participant<R>>(entity).is_some()
    }

    fn targeting<R: Relation>(world: &World, foster: Entity, target: Entity) -> bool {
        let foster_is_targeting = world
            .get::<Edges>(foster)
            .map(|edges| &edges.targets[R::CLEANUP_POLICY as usize])
            .and_then(|bucket| bucket.get(&TypeId::of::<R>()))
            .map_or(false, |set| set.contains(&target));

        let target_is_fostered = world
            .get::<Edges>(target)
            .map(|edges| &edges.fosters[R::CLEANUP_POLICY as usize])
            .and_then(|bucket| bucket.get(&TypeId::of::<R>()))
            .map_or(false, |set| set.contains(&foster));

        if foster_is_targeting != target_is_fostered {
            panic!("Asymmetric edge info");
        }

        foster_is_targeting
    }

    #[test]
    fn set_unset() {
        struct R;

        impl Relation for R {}

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();
        let [foster, target] = from_fn(|_| world.spawn_empty().id());

        world.set::<R>(foster, target);
        assert!(targeting::<R>(&world, foster, target));
        assert!(is_participant::<R>(&world, foster));
        assert!(is_root::<R>(&world, target));

        world.unset::<R>(foster, target);
        assert!(!has_edges(&world, target));
        assert!(!has_edges(&world, foster));
        assert!(!is_participant::<R>(&world, foster));
        assert!(!is_root::<R>(&world, target));
    }

    #[test]
    fn exclusive() {
        struct R;

        impl Relation for R {}

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();
        let [foster, t0, t1] = from_fn(|_| world.spawn_empty().id());

        // Before overwrite
        world.set::<R>(foster, t0);

        assert!(targeting::<R>(&world, foster, t0));
        assert!(is_participant::<R>(&world, foster));
        assert!(is_root::<R>(&world, t0));

        // After overwrite
        world.set::<R>(foster, t1);

        assert!(targeting::<R>(&world, foster, t1));
        assert!(is_participant::<R>(&world, foster));
        assert!(is_root::<R>(&world, t1));

        assert!(!has_edges(&world, t0));
        assert!(!is_root::<R>(&world, t0));
    }

    struct Orphan;
    struct Counted;
    struct Recursive;
    struct Total;

    impl Relation for Orphan {
        const CLEANUP_POLICY: CleanupPolicy = CleanupPolicy::Orphan;
    }

    impl Relation for Counted {
        const CLEANUP_POLICY: CleanupPolicy = CleanupPolicy::Counted;
    }

    impl Relation for Recursive {
        const CLEANUP_POLICY: CleanupPolicy = CleanupPolicy::Recursive;
    }

    impl Relation for Total {
        const CLEANUP_POLICY: CleanupPolicy = CleanupPolicy::Total;
    }

    #[derive(Debug)]
    struct TestEdges {
        orphan: Entity,
        counted: Entity,
        recursive: Entity,
        total: Entity,
    }

    #[derive(Debug)]
    struct Test {
        center: Entity,
        targets: TestEdges,
        fosters: TestEdges,
    }

    impl Test {
        fn new(world: &mut World) -> Self {
            let test = Self {
                center: world.spawn_empty().id(),
                targets: TestEdges {
                    orphan: world.spawn_empty().id(),
                    counted: world.spawn_empty().id(),
                    recursive: world.spawn_empty().id(),
                    total: world.spawn_empty().id(),
                },
                fosters: TestEdges {
                    orphan: world.spawn_empty().id(),
                    counted: world.spawn_empty().id(),
                    recursive: world.spawn_empty().id(),
                    total: world.spawn_empty().id(),
                },
            };

            world.set::<Orphan>(test.fosters.orphan, test.center);
            world.set::<Orphan>(test.center, test.targets.orphan);

            world.set::<Counted>(test.fosters.counted, test.center);
            world.set::<Counted>(test.center, test.targets.counted);

            world.set::<Recursive>(test.fosters.recursive, test.center);
            world.set::<Recursive>(test.center, test.targets.recursive);

            world.set::<Total>(test.fosters.total, test.center);
            world.set::<Total>(test.center, test.targets.total);

            test
        }

        fn assert_unchanged(&self, world: &World) {
            assert!(targeting::<Orphan>(world, self.fosters.orphan, self.center));
            assert!(targeting::<Orphan>(world, self.center, self.targets.orphan));
            assert!(is_participant::<Orphan>(world, self.fosters.orphan,));
            assert!(is_root::<Orphan>(world, self.targets.orphan));

            assert!(targeting::<Counted>(
                world,
                self.fosters.counted,
                self.center
            ));
            assert!(targeting::<Counted>(
                world,
                self.center,
                self.targets.counted
            ));
            assert!(is_participant::<Counted>(world, self.fosters.counted,));
            assert!(is_root::<Counted>(world, self.targets.counted));

            assert!(targeting::<Recursive>(
                world,
                self.fosters.recursive,
                self.center
            ));
            assert!(targeting::<Recursive>(
                world,
                self.center,
                self.targets.recursive
            ));
            assert!(is_participant::<Recursive>(world, self.fosters.recursive,));
            assert!(is_root::<Recursive>(world, self.targets.recursive));

            assert!(targeting::<Total>(world, self.fosters.total, self.center));
            assert!(targeting::<Total>(world, self.center, self.targets.total));
            assert!(is_participant::<Total>(world, self.fosters.total));
            assert!(is_root::<Total>(world, self.targets.total));

            assert!(is_participant::<Orphan>(world, self.center));
            assert!(is_participant::<Counted>(world, self.center));
            assert!(is_participant::<Recursive>(world, self.center));
            assert!(is_participant::<Total>(world, self.center));
        }

        fn assert_cleaned(&self, world: &World) {
            assert!(world.get_entity(self.center).is_none());

            assert!(!has_edges(world, self.fosters.orphan));
            assert!(!has_edges(world, self.targets.orphan));
            assert!(!is_participant::<Orphan>(world, self.fosters.orphan));
            assert!(!is_root::<Orphan>(world, self.targets.orphan));

            assert!(world.get_entity(self.targets.counted).is_none());
            assert!(!has_edges(world, self.fosters.counted));
            assert!(!is_participant::<Counted>(world, self.fosters.counted,));

            assert!(world.get_entity(self.fosters.recursive).is_none());
            assert!(!has_edges(world, self.targets.recursive));
            assert!(!is_root::<Recursive>(world, self.targets.recursive));

            assert!(world.get_entity(self.fosters.total).is_none());
            assert!(world.get_entity(self.targets.total).is_none());
        }
    }

    #[test]
    fn orphan_in_despawned() {
        struct R;

        impl Relation for R {
            const CLEANUP_POLICY: CleanupPolicy = CleanupPolicy::Orphan;
        }

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.set::<R>(e, test.center);

        world.checked_despawn(e);
        test.assert_unchanged(&world);
        assert!(!is_participant::<R>(&world, test.center));
    }

    #[test]
    fn orphan_out_despawned() {
        struct R;

        impl Relation for R {
            const CLEANUP_POLICY: CleanupPolicy = CleanupPolicy::Orphan;
        }

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.set::<R>(test.center, e);

        world.checked_despawn(e);
        test.assert_unchanged(&world);
        assert!(!is_participant::<R>(&world, test.center));
    }

    #[test]
    fn counted_in_despawned() {
        struct R;

        impl Relation for R {
            const CLEANUP_POLICY: CleanupPolicy = CleanupPolicy::Counted;
        }

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.set::<R>(e, test.center);

        world.checked_despawn(e);
        test.assert_cleaned(&world);
    }

    #[test]
    fn counted_out_despawned() {
        struct R;

        impl Relation for R {
            const CLEANUP_POLICY: CleanupPolicy = CleanupPolicy::Counted;
        }

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.set::<R>(test.center, e);

        world.checked_despawn(e);
        test.assert_unchanged(&world);
        assert!(!is_participant::<R>(&world, test.center));
    }

    #[test]
    fn recursive_in_despawned() {
        struct R;

        impl Relation for R {
            const CLEANUP_POLICY: CleanupPolicy = CleanupPolicy::Recursive;
        }

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.set::<R>(e, test.center);

        world.checked_despawn(e);
        test.assert_unchanged(&world);
        assert!(!is_participant::<R>(&world, test.center));
    }

    #[test]
    fn recursive_out_despawned() {
        struct R;

        impl Relation for R {
            const CLEANUP_POLICY: CleanupPolicy = CleanupPolicy::Recursive;
        }

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.set::<R>(test.center, e);

        world.checked_despawn(e);
        test.assert_cleaned(&world);
    }

    #[test]
    fn total_in_despawned() {
        struct R;

        impl Relation for R {
            const CLEANUP_POLICY: CleanupPolicy = CleanupPolicy::Total;
        }

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.set::<R>(e, test.center);

        world.checked_despawn(e);
        test.assert_cleaned(&world);
    }

    #[test]
    fn total_out_despawned() {
        struct R;

        impl Relation for R {
            const CLEANUP_POLICY: CleanupPolicy = CleanupPolicy::Total;
        }

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.set::<R>(test.center, e);

        world.checked_despawn(e);
        test.assert_cleaned(&world);
    }

    #[test]
    fn orphan_in_unset() {
        struct R;

        impl Relation for R {
            const CLEANUP_POLICY: CleanupPolicy = CleanupPolicy::Orphan;
        }

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.set::<R>(e, test.center);
        world.unset::<R>(e, test.center);

        test.assert_unchanged(&world);
        assert!(!is_participant::<R>(&world, test.center));
    }

    #[test]
    fn orphan_out_unset() {
        struct R;

        impl Relation for R {
            const CLEANUP_POLICY: CleanupPolicy = CleanupPolicy::Orphan;
        }

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.set::<R>(test.center, e);
        world.unset::<R>(test.center, e);

        test.assert_unchanged(&world);
        assert!(!is_participant::<R>(&world, test.center));
    }

    #[test]
    fn counted_in_unset() {
        struct R;

        impl Relation for R {
            const CLEANUP_POLICY: CleanupPolicy = CleanupPolicy::Counted;
        }

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.set::<R>(e, test.center);
        world.unset::<R>(e, test.center);

        test.assert_cleaned(&world);
    }

    #[test]
    fn counted_out_unset() {
        struct R;

        impl Relation for R {
            const CLEANUP_POLICY: CleanupPolicy = CleanupPolicy::Counted;
        }

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.set::<R>(test.center, e);
        world.unset::<R>(test.center, e);

        test.assert_unchanged(&world);
        assert!(!is_participant::<R>(&world, test.center));
    }

    #[test]
    fn recursive_in_unset() {
        struct R;

        impl Relation for R {
            const CLEANUP_POLICY: CleanupPolicy = CleanupPolicy::Recursive;
        }

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.set::<R>(e, test.center);
        world.unset::<R>(e, test.center);

        test.assert_unchanged(&world);
        assert!(!is_participant::<R>(&world, test.center));
    }

    #[test]
    fn recursive_out_unset() {
        struct R;

        impl Relation for R {
            const CLEANUP_POLICY: CleanupPolicy = CleanupPolicy::Recursive;
        }

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.set::<R>(test.center, e);
        world.unset::<R>(test.center, e);

        test.assert_cleaned(&world);
    }

    #[test]
    fn total_in_unset() {
        struct R;

        impl Relation for R {
            const CLEANUP_POLICY: CleanupPolicy = CleanupPolicy::Total;
        }

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.set::<R>(e, test.center);
        world.unset::<R>(e, test.center);

        test.assert_cleaned(&world);
    }

    #[test]
    fn total_out_unset() {
        struct R;

        impl Relation for R {
            const CLEANUP_POLICY: CleanupPolicy = CleanupPolicy::Total;
        }

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.set::<R>(test.center, e);
        world.unset::<R>(test.center, e);

        test.assert_cleaned(&world);
    }
}
