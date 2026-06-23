use express_it::expr::{Context, Expr};
use express_it::logic::*;
use express_it::nodes::Node;

#[derive(Debug)]
pub struct PlayerStats {
    pub health: i32,
    pub max_health: i32,
    pub level: i32,
    pub mana: f32,
    pub poisoned: bool,
    pub berserk: bool,
}

pub struct GameCtx;

impl Context for GameCtx {
    type ContextItem<'w, 's> = PlayerStats;
}

fn main() {
    let player = PlayerStats {
        health: 60,
        max_health: 100,
        level: 12,
        mana: 45.5,
        poisoned: true,
        berserk: false,
    };

    println!("=== Expression Display Showcase ===\n");
    println!("Player: {:?}\n", player);

    // 1. Comparisons
    println!("--- Comparisons ---");
    let low_health = Node::<i32, GameCtx, _>::new(|ctx: &PlayerStats| ctx.health).lt(50);
    println!("  expr : {}", low_health);
    println!("  value: {}\n", low_health.eval(&player));

    let above_half = Node::<i32, GameCtx, _>::new(|ctx: &PlayerStats| ctx.health).gt(50);
    println!("  expr : {}", above_half);
    println!("  value: {}\n", above_half.eval(&player));

    let exact = Node::<i32, GameCtx, _>::new(|ctx: &PlayerStats| ctx.level).eq(12);
    println!("  expr : {}", exact);
    println!("  value: {}\n", exact.eval(&player));

    let not_berserk = Node::<bool, GameCtx, _>::new(|ctx: &PlayerStats| ctx.berserk).ne(true);
    println!("  expr : {}", not_berserk);
    println!("  value: {}\n", not_berserk.eval(&player));

    let high_mana = Node::<f32, GameCtx, _>::new(|ctx: &PlayerStats| ctx.mana).ge(50.0);
    println!("  expr : {}", high_mana);
    println!("  value: {}\n", high_mana.eval(&player));

    let enough_mana = Node::<f32, GameCtx, _>::new(|ctx: &PlayerStats| ctx.mana).le(100.0);
    println!("  expr : {}", enough_mana);
    println!("  value: {}\n", enough_mana.eval(&player));

    // 2. Negation
    println!("--- Negation ---");
    let is_full = Node::<i32, GameCtx, _>::new(|ctx: &PlayerStats| ctx.health).eq(100);
    let not_full = is_full.not();
    println!("  expr : {}", is_full);
    println!("  value: {}", is_full.eval(&player));
    println!("  expr : {}", not_full);
    println!("  value: {}\n", not_full.eval(&player));

    // 3. Logical operators
    println!("--- Logical Operators ---");
    let is_hurt = Node::<i32, GameCtx, _>::new(|ctx: &PlayerStats| ctx.health).lt(100);
    let has_mana = Node::<f32, GameCtx, _>::new(|ctx: &PlayerStats| ctx.mana).gt(0.0);

    let can_fight = is_hurt.and(has_mana);
    println!("  expr : {}", can_fight);
    println!("  value: {}\n", can_fight.eval(&player));

    let invulnerable = is_hurt.or(has_mana);
    println!("  expr : {}", invulnerable);
    println!("  value: {}\n", invulnerable.eval(&player));

    let weird = is_hurt.xor(has_mana);
    println!("  expr : {}", weird);
    println!("  value: {}\n", weird.eval(&player));

    // 4. Complex chain
    println!("--- Complex Condition Chain ---");
    let critical = Node::<i32, GameCtx, _>::new(|ctx: &PlayerStats| ctx.health)
        .lt(30)
        .and(Node::<bool, GameCtx, _>::new(|ctx: &PlayerStats| {
            ctx.poisoned
        }))
        .or(
            Node::<bool, GameCtx, _>::new(|ctx: &PlayerStats| ctx.berserk)
                .and(Node::<i32, GameCtx, _>::new(|ctx: &PlayerStats| ctx.level).ge(10)),
        );

    println!("  description: (health < 30 AND poisoned) OR (berserk AND level >= 10)");
    println!("  value: {}", critical.eval(&player));
    println!("  display: {}\n", critical);

    // 5. Cross-type chain
    println!("--- Cross-Type Expression Chain ---");
    let half_hp = Node::<i32, GameCtx, _>::new(|ctx: &PlayerStats| ctx.max_health) / 2;
    let below_half = Node::<i32, GameCtx, _>::new(|ctx: &PlayerStats| ctx.health).lt(half_hp);
    let low_mana = Node::<f32, GameCtx, _>::new(|ctx: &PlayerStats| ctx.mana).le(30.0);
    let struggling = below_half.and(low_mana);
    println!("  description: health < max_health/2 AND mana <= 30.0");
    println!("  value: {}", struggling.eval(&player));
    println!("  display: {}\n", struggling);

    // 6. Nested not + and/or
    println!("--- Nested Not + And/Or ---");
    let safe = Node::<i32, GameCtx, _>::new(|ctx: &PlayerStats| ctx.health)
        .ge(20)
        .and(Node::<bool, GameCtx, _>::new(|ctx: &PlayerStats| ctx.poisoned).not());
    println!("  description: health >= 20 AND !poisoned");
    println!("  value: {}", safe.eval(&player));
    println!("  display: {}\n", safe);

    // 7. Bool literals
    println!("--- Bool Literals as Expressions ---");
    let always_true = Node::<bool, GameCtx, _>::lit(true).and(false).or(true);
    println!("  description: true AND false OR true");
    println!("  value: {}", always_true.eval(&player));
    println!("  display: {}\n", always_true);

    // 8. Arithmetic expressions
    println!("--- Arithmetic Expressions ---");
    let damage_expr = Node::<i32, GameCtx, _>::new(|ctx: &PlayerStats| ctx.health) + 10;
    println!("  description: health + 10");
    println!("  display: {}\n", damage_expr);

    let heal_expr = (Node::<i32, GameCtx, _>::new(|ctx: &PlayerStats| ctx.max_health) - 50) / 2;
    println!("  description: (max_health - 50) / 2");
    println!("  display: {}\n", heal_expr);

    println!("=== Done ===");
}
