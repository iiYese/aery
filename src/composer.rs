use crate::relation::{CheckedDespawn, Relation, Target, Untarget, UntargetAll, Withdraw};
use std::marker::PhantomData;

use bevy::ecs::{
    bundle::Bundle,
    entity::Entity,
    system::{Command, Commands},
    world::{EntityMut, EntityRef, World},
};

pub trait Compose {
    fn compose(&mut self, bundle: impl Bundle) -> Composer<&'_ mut Self>;
    fn update(&mut self, entity: Entity) -> Composer<&'_ mut Self>;
}

impl Compose for World {
    fn compose(&mut self, bundle: impl Bundle) -> Composer<&'_ mut Self> {
        Composer {
            entity: self.spawn(bundle).id(),
            world_api: self,
        }
    }

    fn update(&mut self, entity: Entity) -> Composer<&'_ mut Self> {
        Composer {
            entity,
            world_api: self,
        }
    }
}

pub struct Composer<Api> {
    world_api: Api,
    entity: Entity,
}

impl Compose for Commands<'_, '_> {
    fn compose(&mut self, bundle: impl Bundle) -> Composer<&'_ mut Self> {
        Composer {
            entity: self.spawn(bundle).id(),
            world_api: self,
        }
    }

    fn update(&mut self, entity: Entity) -> Composer<&'_ mut Self> {
        Composer {
            entity,
            world_api: self,
        }
    }
}

impl Composer<&'_ mut World> {
    pub fn untarget<R: Relation>(&mut self, target: Entity) -> &'_ mut Self {
        Command::write(
            Untarget::<R> {
                target,
                host: self.entity,
                _phantom: PhantomData,
            },
            self.world_api,
        );

        self
    }

    pub fn untarget_all<R: Relation>(&mut self) -> &'_ mut Self {
        Command::write(
            UntargetAll::<R> {
                entity: self.entity,
                _phantom: PhantomData,
            },
            self.world_api,
        );

        self
    }

    pub fn withdraw<R: Relation>(&mut self) -> &'_ mut Self {
        Command::write(
            Withdraw::<R> {
                entity: self.entity,
                _phantom: PhantomData,
            },
            self.world_api,
        );

        self
    }

    pub fn target<R: Relation>(&mut self, entity: Entity) -> &mut Self {
        Command::write(
            Target::<R> {
                host: self.entity,
                target: entity,
                _phantom: PhantomData,
            },
            self.world_api,
        );
        self
    }

    pub fn descendant<R: Relation>(
        &mut self,
        descendant: Entity,
        mut func: impl FnMut(&'_ mut Self),
    ) -> &'_ mut Self {
        Command::write(
            Target::<R> {
                host: descendant,
                target: self.entity,
                _phantom: PhantomData,
            },
            self.world_api,
        );

        let curr = self.entity;
        self.entity = descendant;
        func(self);

        self.entity = curr;
        self
    }

    pub fn spawn_target<R: Relation>(&mut self, bundle: impl Bundle) -> &mut Self {
        let entity = self.world_api.spawn(bundle).id();
        self.target::<R>(entity)
    }

    pub fn spawn_descendant<R: Relation>(
        &mut self,
        descendant: impl Bundle,
        func: impl FnMut(&'_ mut Self),
    ) -> &'_ mut Self {
        let descendant = self.world_api.spawn(descendant).id();
        self.descendant::<R>(descendant, func)
    }

    pub fn checked_despawn(&mut self) {
        Command::write(
            CheckedDespawn {
                entity: self.entity,
            },
            self.world_api,
        );
    }
}

impl Composer<&'_ mut Commands<'_, '_>> {
    pub fn untarget<R: Relation>(&mut self, target: Entity) -> &'_ mut Self {
        self.world_api.add(Untarget::<R> {
            target,
            host: self.entity,
            _phantom: PhantomData,
        });

        self
    }

    pub fn untarget_all<R: Relation>(&mut self) -> &'_ mut Self {
        self.world_api.add(UntargetAll::<R> {
            entity: self.entity,
            _phantom: PhantomData,
        });

        self
    }

    pub fn withdraw<R: Relation>(&mut self) -> &'_ mut Self {
        self.world_api.add(Withdraw::<R> {
            entity: self.entity,
            _phantom: PhantomData,
        });

        self
    }

    pub fn target<R: Relation>(&mut self, entity: Entity) -> &mut Self {
        self.world_api.add(Target::<R> {
            host: self.entity,
            target: entity,
            _phantom: PhantomData,
        });
        self
    }

    pub fn descendant<R: Relation>(
        &mut self,
        descendant: Entity,
        mut func: impl FnMut(&'_ mut Self),
    ) -> &'_ mut Self {
        self.world_api.add(Target::<R> {
            host: descendant,
            target: self.entity,
            _phantom: PhantomData,
        });

        let curr = self.entity;
        self.entity = descendant;
        func(self);

        self.entity = curr;
        self
    }

    pub fn spawn_target<R: Relation>(&mut self, bundle: impl Bundle) -> &mut Self {
        let entity = self.world_api.spawn(bundle).id();
        self.target::<R>(entity)
    }

    pub fn spawn_descendant<R: Relation>(
        &mut self,
        descendant: impl Bundle,
        func: impl FnMut(&'_ mut Self),
    ) -> &'_ mut Self {
        let descendant = self.world_api.spawn(descendant).id();
        self.descendant::<R>(descendant, func)
    }

    pub fn checked_despawn(&mut self) {
        self.world_api.add(CheckedDespawn {
            entity: self.entity,
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        prelude::*,
        relation::{Edges, Participant, RefragmentHooks, RootMarker},
    };
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

    fn targeting<R: Relation>(world: &World, host: Entity, target: Entity) -> bool {
        let host_is_targeting = world
            .get::<Edges>(host)
            .map(|edges| &edges.targets[R::CLEANUP_POLICY as usize])
            .and_then(|bucket| bucket.get(&TypeId::of::<R>()))
            .map_or(false, |set| set.contains(&target));

        let target_is_hosted = world
            .get::<Edges>(target)
            .map(|edges| &edges.hosts[R::CLEANUP_POLICY as usize])
            .and_then(|bucket| bucket.get(&TypeId::of::<R>()))
            .map_or(false, |set| set.contains(&host));

        if host_is_targeting != target_is_hosted {
            panic!("Asymmetric edge info");
        }

        host_is_targeting
    }

    #[test]
    fn target_untarget() {
        #[derive(Relation)]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();
        let [host, target] = from_fn(|_| world.spawn_empty().id());

        world.update(host).target::<R>(target);
        assert!(targeting::<R>(&world, host, target));
        assert!(is_participant::<R>(&world, host));
        assert!(is_root::<R>(&world, target));

        world.update(host).untarget::<R>(target);
        assert!(!has_edges(&world, target));
        assert!(!has_edges(&world, host));
        assert!(!is_participant::<R>(&world, host));
        assert!(!is_root::<R>(&world, target));
    }

    #[test]
    fn exclusive() {
        #[derive(Relation)]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();
        let [host, t0, t1] = from_fn(|_| world.spawn_empty().id());

        // Before overwrite
        world.update(host).target::<R>(t0);

        assert!(targeting::<R>(&world, host, t0));
        assert!(is_participant::<R>(&world, host));
        assert!(is_root::<R>(&world, t0));

        // After overwrite
        world.update(host).target::<R>(t1);

        assert!(targeting::<R>(&world, host, t1));
        assert!(is_participant::<R>(&world, host));
        assert!(is_root::<R>(&world, t1));

        assert!(!has_edges(&world, t0));
        assert!(!is_root::<R>(&world, t0));
    }

    #[derive(Relation)]
    #[cleanup(policy = "Orphan")]
    struct Orphan;

    #[derive(Relation)]
    #[cleanup(policy = "Counted")]
    struct Counted;

    #[derive(Relation)]
    #[cleanup(policy = "Recursive")]
    struct Recursive;

    #[derive(Relation)]
    #[cleanup(policy = "Total")]
    struct Total;

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
        hosts: TestEdges,
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
                hosts: TestEdges {
                    orphan: world.spawn_empty().id(),
                    counted: world.spawn_empty().id(),
                    recursive: world.spawn_empty().id(),
                    total: world.spawn_empty().id(),
                },
            };

            world
                .update(test.hosts.orphan)
                .target::<Orphan>(test.center);
            world
                .update(test.center)
                .target::<Orphan>(test.targets.orphan);

            world
                .update(test.hosts.counted)
                .target::<Counted>(test.center);
            world
                .update(test.center)
                .target::<Counted>(test.targets.counted);

            world
                .update(test.hosts.recursive)
                .target::<Recursive>(test.center);
            world
                .update(test.center)
                .target::<Recursive>(test.targets.recursive);

            world.update(test.hosts.total).target::<Total>(test.center);
            world
                .update(test.center)
                .target::<Total>(test.targets.total);

            test
        }

        fn assert_unchanged(&self, world: &World) {
            assert!(targeting::<Orphan>(world, self.hosts.orphan, self.center));
            assert!(targeting::<Orphan>(world, self.center, self.targets.orphan));
            assert!(is_participant::<Orphan>(world, self.hosts.orphan,));
            assert!(is_root::<Orphan>(world, self.targets.orphan));

            assert!(targeting::<Counted>(world, self.hosts.counted, self.center));
            assert!(targeting::<Counted>(
                world,
                self.center,
                self.targets.counted
            ));
            assert!(is_participant::<Counted>(world, self.hosts.counted,));
            assert!(is_root::<Counted>(world, self.targets.counted));

            assert!(targeting::<Recursive>(
                world,
                self.hosts.recursive,
                self.center
            ));
            assert!(targeting::<Recursive>(
                world,
                self.center,
                self.targets.recursive
            ));
            assert!(is_participant::<Recursive>(world, self.hosts.recursive,));
            assert!(is_root::<Recursive>(world, self.targets.recursive));

            assert!(targeting::<Total>(world, self.hosts.total, self.center));
            assert!(targeting::<Total>(world, self.center, self.targets.total));
            assert!(is_participant::<Total>(world, self.hosts.total));
            assert!(is_root::<Total>(world, self.targets.total));

            assert!(is_participant::<Orphan>(world, self.center));
            assert!(is_participant::<Counted>(world, self.center));
            assert!(is_participant::<Recursive>(world, self.center));
            assert!(is_participant::<Total>(world, self.center));
        }

        fn assert_cleaned(&self, world: &World) {
            assert!(world.get_entity(self.center).is_none());

            assert!(!has_edges(world, self.hosts.orphan));
            assert!(!has_edges(world, self.targets.orphan));
            assert!(!is_participant::<Orphan>(world, self.hosts.orphan));
            assert!(!is_root::<Orphan>(world, self.targets.orphan));

            assert!(world.get_entity(self.targets.counted).is_none());
            assert!(!has_edges(world, self.hosts.counted));
            assert!(!is_participant::<Counted>(world, self.hosts.counted,));

            assert!(world.get_entity(self.hosts.recursive).is_none());
            assert!(!has_edges(world, self.targets.recursive));
            assert!(!is_root::<Recursive>(world, self.targets.recursive));

            assert!(world.get_entity(self.hosts.total).is_none());
            assert!(world.get_entity(self.targets.total).is_none());
        }
    }

    #[test]
    fn orphan_in_despawned() {
        #[derive(Relation)]
        #[cleanup(policy = "Orphan")]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.update(e).target::<R>(test.center).checked_despawn();

        test.assert_unchanged(&world);
        assert!(!is_participant::<R>(&world, test.center));
    }

    #[test]
    fn orphan_out_despawned() {
        #[derive(Relation)]
        #[cleanup(policy = "Orphan")]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.update(test.center).target::<R>(e);

        world.update(e).checked_despawn();
        test.assert_unchanged(&world);
        assert!(!is_participant::<R>(&world, test.center));
    }

    #[test]
    fn counted_in_despawned() {
        #[derive(Relation)]
        #[cleanup(policy = "Counted")]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.update(e).target::<R>(test.center).checked_despawn();

        test.assert_cleaned(&world);
    }

    #[test]
    fn counted_out_despawned() {
        #[derive(Relation)]
        #[cleanup(policy = "Counted")]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.update(test.center).target::<R>(e);

        world.update(e).checked_despawn();
        test.assert_unchanged(&world);
        assert!(!is_participant::<R>(&world, test.center));
    }

    #[test]
    fn recursive_in_despawned() {
        #[derive(Relation)]
        #[cleanup(policy = "Recursive")]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.update(e).target::<R>(test.center).checked_despawn();

        test.assert_unchanged(&world);
        assert!(!is_participant::<R>(&world, test.center));
    }

    #[test]
    fn recursive_out_despawned() {
        #[derive(Relation)]
        #[cleanup(policy = "Recursive")]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.update(test.center).target::<R>(e);

        world.update(e).checked_despawn();
        test.assert_cleaned(&world);
    }

    #[test]
    fn total_in_despawned() {
        #[derive(Relation)]
        #[cleanup(policy = "Total")]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.update(e).target::<R>(test.center).checked_despawn();

        test.assert_cleaned(&world);
    }

    #[test]
    fn total_out_despawned() {
        #[derive(Relation)]
        #[cleanup(policy = "Total")]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.update(test.center).target::<R>(e);

        world.update(e).checked_despawn();
        test.assert_cleaned(&world);
    }

    #[test]
    fn orphan_in_untarget() {
        #[derive(Relation)]
        #[cleanup(policy = "Orphan")]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world
            .update(e)
            .target::<R>(test.center)
            .untarget::<R>(test.center);

        test.assert_unchanged(&world);
        assert!(!is_participant::<R>(&world, test.center));
    }

    #[test]
    fn orphan_out_untarget() {
        #[derive(Relation)]
        #[cleanup(policy = "Orphan")]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.update(test.center).target::<R>(e).untarget::<R>(e);

        test.assert_unchanged(&world);
        assert!(!is_participant::<R>(&world, test.center));
    }

    #[test]
    fn counted_in_untarget() {
        #[derive(Relation)]
        #[cleanup(policy = "Counted")]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world
            .update(e)
            .target::<R>(test.center)
            .untarget::<R>(test.center);

        test.assert_cleaned(&world);
    }

    #[test]
    fn counted_out_untarget() {
        #[derive(Relation)]
        #[cleanup(policy = "Counted")]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.update(test.center).target::<R>(e).untarget::<R>(e);

        test.assert_unchanged(&world);
        assert!(!is_participant::<R>(&world, test.center));
    }

    #[test]
    fn recursive_in_untarget() {
        #[derive(Relation)]
        #[cleanup(policy = "Recursive")]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world
            .update(e)
            .target::<R>(test.center)
            .untarget::<R>(test.center);

        test.assert_unchanged(&world);
        assert!(!is_participant::<R>(&world, test.center));
    }

    #[test]
    fn recursive_out_untarget() {
        #[derive(Relation)]
        #[cleanup(policy = "Recursive")]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.update(test.center).target::<R>(e).untarget::<R>(e);

        test.assert_cleaned(&world);
    }

    #[test]
    fn total_in_untarget() {
        #[derive(Relation)]
        #[cleanup(policy = "Total")]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world
            .update(e)
            .target::<R>(test.center)
            .untarget::<R>(test.center);

        test.assert_cleaned(&world);
    }

    #[test]
    fn total_out_untarget() {
        #[derive(Relation)]
        #[cleanup(policy = "Total")]
        struct R;

        let mut world = World::new();
        world.init_resource::<RefragmentHooks>();

        let test = Test::new(&mut world);

        let e = world.spawn_empty().id();
        world.update(test.center).target::<R>(e).untarget::<R>(e);

        test.assert_cleaned(&world);
    }
}
