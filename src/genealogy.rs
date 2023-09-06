use bevy::ecs::{component::Component, query::WorldQuery};
use std::marker::PhantomData;

/// By default all components are hereditary meaning they are always passed down to descendants.
/// The presense of an Innate<C> on an enitity results in the following properties:
///     - The `Innate<C>` of an entity will not be passed down to descendants.
///     - Ancestry queries will prefer the `Innate<C>` over the inherited `C`.
///     - An `Innate<C>` does not prevent the passing down of a `C` from an ancestor.
///     - An `Innate<C>` does not prevent the passing down of a `C` on the entity.
#[derive(Component)]
pub struct Innate<T: Component>(pub T);

pub struct Dna<Inherited, Innate> {
    inherited: Inherited,
    innate: Innate,
}

pub trait LineageQuery {
    type InnateQuery;
}

pub struct Lineage<R, Components: LineageQuery> {
    inherited: Components,
    innate: Components::InnateQuery,
    _phantom: PhantomData<R>,
}
