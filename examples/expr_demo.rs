//! Variables over an entity store: the pattern a game engine wants, with no engine
//! dependency.
//!
//! The crate is engine-agnostic on purpose. What an ECS needs from it is one thing
//! — a context *item* that borrows the world and looks components up by type — and
//! that fits the GAT `Context::ContextItem<'w, 's>` exactly the way it fits an
//! engine's own entity reference: `'w` for the world borrow, `'s` for the shorter
//! borrow the system handed you. This file used to prove that against bevy; the
//! engine is gone and the shape it demonstrated is still the point, so the store
//! below is hand-written in about fifty lines.
//!
//! Run it with `cargo run --example expr_demo`.

use std::any::{Any, type_name};
use std::collections::HashMap;

use express_it::expr::{Context, Expr};
use express_it::nodes::Node;
use express_it::ops::ExprClamp;

// ---------------------------------------------------------------------------
// The stand-in world: entities are `u32`, components live in a per-entity map
// ---------------------------------------------------------------------------

struct Health {
    value: f32,
}
struct Damage {
    value: f32,
}
struct UnsignedAttr(u32);

#[derive(Default)]
struct World {
    entities: HashMap<u32, Components>,
}

#[derive(Default)]
struct Components {
    parts: HashMap<&'static str, Box<dyn Any>>,
}

impl Components {
    fn insert<T: Any>(&mut self, component: T) {
        self.parts.insert(type_name::<T>(), Box::new(component));
    }

    fn get<T: Any>(&self) -> Option<&T> {
        self.parts
            .get(type_name::<T>())
            .and_then(|part| part.downcast_ref::<T>())
    }
}

impl World {
    fn spawn(&mut self, id: u32) -> &mut Components {
        self.entities.entry(id).or_default()
    }

    fn entity(&self, id: u32) -> Entity<'_> {
        Entity { world: self, id }
    }
}

/// A read view over one entity, borrowed for as long as the world is.
#[derive(Clone, Copy)]
struct Entity<'w> {
    world: &'w World,
    id: u32,
}

impl<'w> Entity<'w> {
    fn get<T: Any>(&self) -> Option<&T> {
        self.world
            .entities
            .get(&self.id)
            .and_then(|parts| parts.get::<T>())
    }

    /// The same view, told to refuse one component by name — the shape an engine
    /// uses to hand a system a view it cannot accidentally read through. It is the
    /// only part here that needs the second lifetime, which is why the crate's
    /// context item carries two.
    fn except<'s>(self, component: &'s str) -> EntityExcept<'w, 's> {
        EntityExcept {
            entity: self,
            component,
        }
    }
}

#[derive(Clone, Copy)]
struct EntityExcept<'w, 's> {
    entity: Entity<'w>,
    component: &'s str,
}

impl<'w> EntityExcept<'w, '_> {
    fn get<T: Any>(&self) -> Option<&T> {
        if type_name::<T>().ends_with(self.component) {
            None
        } else {
            self.entity.get::<T>()
        }
    }
}

/// The context *type* carries nothing; everything travels in the item.
struct GameCtx;

impl Context for GameCtx {
    type ContextItem<'w, 's> = GameItem<'w, 's>;
}

struct GameItem<'w, 's> {
    src: Entity<'w>,
    dst: Entity<'w>,
    val: EntityExcept<'w, 's>,
}

// ---------------------------------------------------------------------------
// The expressions
// ---------------------------------------------------------------------------

fn main() {
    let mut world = World::default();
    world.spawn(1).insert(Health { value: 100.0 });
    world.spawn(1).insert(Damage { value: 10.0 });
    world.spawn(1).insert(UnsignedAttr(10));

    let player = world.entity(1);
    let ctx = GameItem {
        src: player,
        dst: player,
        val: player.except("Health"),
    };

    // A variable reads a component off whichever entity the item points at. A
    // missing component reads 0.0 rather than failing, which is the usual game
    // convention; `Option` handling belongs in the fetch function.
    let player_health = Node::<f32, GameCtx, _>::var("player_health", |ctx| {
        ctx.src.get::<Health>().map_or(0.0, |health| health.value)
    });
    let damage = Node::<f32, GameCtx, _>::var("damage", |ctx| {
        ctx.dst.get::<Damage>().map_or(0.0, |damage| damage.value)
    });

    let expr = 25.0f32 + player_health * damage + 10.0 + -player_health;
    println!("Player health: {}", expr.eval(&ctx));
    println!("  as text:     {expr}");

    // Casting follows Rust's `as`, so an unsigned attribute becomes a float
    // exactly where the language would.
    let unsigned = Node::<u32, GameCtx, _>::var("unsigned", |ctx| {
        ctx.dst.get::<UnsignedAttr>().map_or(0, |attr| attr.0)
    });
    let result = unsigned.cast::<f32>().eval(&ctx);
    println!("Unsigned attr: {result:?}");

    // A number that comes out of somewhere else in the game — an ability table, a
    // crit roll. It is large enough that all three clamps below actually bite,
    // which is the point of the demo.
    let raw_damage = Node::<f32, GameCtx, _>::var("raw_damage", |ctx| {
        ctx.dst
            .get::<Damage>()
            .map_or(0.0, |damage| damage.value * 15.0)
    });
    let min_cap = Node::<f32, GameCtx, _>::var("min_cap", |_| 10.0);
    let max_cap = Node::<f32, GameCtx, _>::var("max_cap", |_| 125.0);

    // 1. Bounds written as literals.
    let safe_damage_lit = raw_damage.clamp(0.0, 100.0);
    println!(
        "Safe damage (literal bounds): {:?}",
        safe_damage_lit.eval(&ctx)
    );

    // 2. Bounds that are themselves variables, so tuning data can drive them.
    let safe_damage_dyn = raw_damage.clamp(min_cap, max_cap);
    println!(
        "Safe damage (dynamic bounds): {:?}",
        safe_damage_dyn.eval(&ctx)
    );

    // 3. Nesting inside clamp needs no extra ceremony.
    let complex_calculation = (raw_damage * 2.0).clamp(0.0, 500.0) + 5.0;
    println!("Complex calculation: {:?}", complex_calculation.eval(&ctx));

    // The component this view may not read: the fetch asks for it and gets the
    // default, because `except("Health")` refuses it.
    let blinded = Node::<f32, GameCtx, _>::var("blinded_health", |ctx| {
        ctx.val.get::<Health>().map_or(0.0, |health| health.value)
    });
    let still_readable = Node::<f32, GameCtx, _>::var("damage_through_view", |ctx| {
        ctx.val.get::<Damage>().map_or(0.0, |damage| damage.value)
    });
    println!(
        "Through the restricted view: health {} (refused), damage {} (allowed)",
        blinded.eval(&ctx),
        still_readable.eval(&ctx)
    );
}
