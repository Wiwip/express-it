use express_it::expr::{Context, Expr};
use express_it::logic::*;
use express_it::nodes::Node;
use express_it::{all, any};

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

    println!("=== Logic System Showcase ===\n");
    println!("Player: {:?}\n", player);

    // 1. Basic comparisons
    println!("--- Comparisons ---");
    let low_health = Node::<i32, GameCtx, _>::new(|ctx: &PlayerStats| ctx.health).lt(50);
    println!("health < 50         : {}", low_health.eval(&player));

    let above_half = Node::<i32, GameCtx, _>::new(|ctx: &PlayerStats| ctx.health).gt(50);
    println!("health > 50         : {}", above_half.eval(&player));

    let exact = Node::<i32, GameCtx, _>::new(|ctx: &PlayerStats| ctx.level).eq(12);
    println!("level == 12         : {}", exact.eval(&player));

    let not_berserk = Node::<bool, GameCtx, _>::new(|ctx: &PlayerStats| ctx.berserk).ne(true);
    println!("berserk != true     : {}", not_berserk.eval(&player));

    let high_mana = Node::<f32, GameCtx, _>::new(|ctx: &PlayerStats| ctx.mana).ge(50.0);
    println!("mana >= 50.0        : {}", high_mana.eval(&player));

    let enough_mana = Node::<f32, GameCtx, _>::new(|ctx: &PlayerStats| ctx.mana).le(100.0);
    println!("mana <= 100.0       : {}\n", enough_mana.eval(&player));

    // 2. Negation
    println!("--- Negation ---");
    let is_full = Node::<i32, GameCtx, _>::new(|ctx: &PlayerStats| ctx.health).eq(100);
    let not_full = is_full.not();
    println!("health == 100       : {}", is_full.eval(&player));
    println!("!(health == 100)    : {}\n", not_full.eval(&player));

    // 3. Logical operators
    println!("--- Logical Operators ---");
    let is_hurt = Node::<i32, GameCtx, _>::new(|ctx: &PlayerStats| ctx.health).lt(100);
    let has_mana = Node::<f32, GameCtx, _>::new(|ctx: &PlayerStats| ctx.mana).gt(0.0);

    let can_fight = is_hurt.and(has_mana);
    println!("is_hurt AND has_mana : {}", can_fight.eval(&player));

    let invulnerable = is_hurt.or(has_mana);
    println!("is_hurt OR has_mana  : {}", invulnerable.eval(&player));

    let weird = is_hurt.xor(has_mana);
    println!("is_hurt XOR has_mana : {}\n", weird.eval(&player));

    let nand_expr = is_hurt.nand(has_mana);
    println!("is_hurt NAND has_mana: {}", nand_expr.eval(&player));

    let nor_expr = is_hurt.nor(has_mana);
    println!("is_hurt NOR  has_mana: {}\n", nor_expr.eval(&player));

    let nxor_expr = is_hurt.nxor(has_mana);
    println!("is_hurt NXOR has_mana: {}\n", nxor_expr.eval(&player));

    // 4. Complex chain: (health < 30 AND poisoned) OR (berserk AND level >= 10)
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

    println!(
        "(health < 30 AND poisoned) OR (berserk AND level >= 10): {}\n",
        critical.eval(&player)
    );

    // 5. Mixed-type chaining: health < (max_health / 2) AND mana <= 30.0
    println!("--- Cross-Type Expression Chain ---");
    let half_hp = Node::<i32, GameCtx, _>::new(|ctx: &PlayerStats| ctx.max_health) / 2;
    let below_half = Node::<i32, GameCtx, _>::new(|ctx: &PlayerStats| ctx.health).lt(half_hp);
    let low_mana = Node::<f32, GameCtx, _>::new(|ctx: &PlayerStats| ctx.mana).le(30.0);
    let struggling = below_half.and(low_mana);
    println!(
        "health < max_health/2 AND mana <= 30.0 : {}\n",
        struggling.eval(&player)
    );

    // 6. Nested not + and/or
    println!("--- Nested Not + And/Or ---");
    let safe = Node::<i32, GameCtx, _>::new(|ctx: &PlayerStats| ctx.health)
        .ge(20)
        .and(Node::<bool, GameCtx, _>::new(|ctx: &PlayerStats| ctx.poisoned).not());
    println!("health >= 20 AND !poisoned : {}\n", safe.eval(&player));

    // 7. Bool literal chaining
    println!("--- Bool Literals as Expressions ---");
    let always_true = Node::<bool, GameCtx, _>::lit(true).and(false).or(true);
    println!("true AND false OR true : {}\n", always_true.eval(&player));

    // 8. all/any macros
    println!("--- All / Any Macros ---");
    let cond1 = Node::<i32, GameCtx, _>::new(|ctx: &PlayerStats| ctx.health).gt(20);
    let cond2 = Node::<bool, GameCtx, _>::new(|ctx: &PlayerStats| ctx.poisoned).not();
    let cond3 = Node::<i32, GameCtx, _>::new(|ctx: &PlayerStats| ctx.level).ge(5);

    let all_conditions = all![cond1, cond2, cond3];
    println!(
        "all[health>20, !poisoned, level>=5] : {}\n",
        all_conditions.eval(&player)
    );

    let any_conditions = any![
        Node::<i32, GameCtx, _>::new(|ctx: &PlayerStats| ctx.health).lt(20),
        Node::<bool, GameCtx, _>::new(|ctx: &PlayerStats| ctx.berserk),
        Node::<i32, GameCtx, _>::new(|ctx: &PlayerStats| ctx.level).ge(20),
    ];
    println!(
        "any[health<20, berserk, level>=20] : {}\n",
        any_conditions.eval(&player)
    );

    println!("=== Done ===");
}
