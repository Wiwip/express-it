use bevy::prelude::*;
use express_it::expr::{Context, Expr};
use express_it::nodes::Var;
use express_it::ops::ExprClamp;

fn main() {
    App::new()
        .add_plugins(MinimalPlugins)
        .add_systems(Startup, setup)
        .add_systems(PostStartup, post_setup)
        .run();
}

fn setup(mut commands: Commands) {
    commands.spawn((
        Health { value: 100.0 },
        Damage { value: 10.0 },
        UnsignedAttr(10),
    ));
}

fn post_setup(query: Query<EntityRef, With<Health>>) {
    let player_health =
        Var::<f32, MyGameCtx>::new(|ctx| ctx.src.get::<Health>().map(|h| h.value).unwrap_or(0.0));

    let damage =
        Var::<f32, MyGameCtx>::new(|ctx| ctx.dst.get::<Damage>().map(|a| a.value).unwrap_or(0.0));

    for entity in query.iter() {
        let ctx = BevyCtx {
            src: entity,
            dst: entity,
        };

        let expr = 25.0 + player_health * damage + 10.0 + -player_health;
        let result = expr.eval(&ctx);
        println!("Player health: {}", result);

        let unsigned = Var::<u32, MyGameCtx>::new(|ctx| {
            ctx.dst.get::<UnsignedAttr>().map(|a| a.0).unwrap_or(0)
        });
        let expr = unsigned.cast::<f32>();
        let result = expr.eval(&ctx);
        println!("Unsigned attr: {:?}", result);

        let raw_damage = Var::<f32, MyGameCtx>::new(|_ctx| 150.0);
        let min_cap = Var::<f32, MyGameCtx>::new(|_ctx| 10.0);
        let max_cap = Var::<f32, MyGameCtx>::new(|_ctx| 125.0);

        // 1. Clamping via purely static literal bounds (Extremely common)
        let safe_damage_lit = raw_damage.clamp(0.0, 100.0);
        println!(
            "Safe damage (literal bounds): {:?}",
            safe_damage_lit.eval(&ctx)
        );

        // 2. Clamping using references to other dynamic system variables
        let safe_damage_dyn = raw_damage.clamp(min_cap, max_cap);
        println!(
            "Safe damage (dynamic bounds): {:?}",
            safe_damage_dyn.eval(&ctx)
        );

        // 3. Complex math nesting right inside clamp works out of the box
        let complex_calculation = (raw_damage * 2.0).clamp(0.0, 500.0) + 5.0;
        println!("Complex calculation: {:?}", complex_calculation.eval(&ctx));
    }
}

#[derive(Component)]
struct Health {
    value: f32,
}

#[derive(Component)]
struct Damage {
    value: f32,
}

#[derive(Component)]
struct UnsignedAttr(u32);

pub struct BevyCtx<'w> {
    pub src: EntityRef<'w>,
    pub dst: EntityRef<'w>,
}

pub struct MyGameCtx;

impl Context for MyGameCtx {
    type ContextItem<'w, 's> = BevyCtx<'w>;
}
