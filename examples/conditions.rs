use express_it::logic::{BoolExpr, CompareExpr};
use express_it::test_utils::scopes::SRC;
use express_it::test_utils::{Atk, Hp, IntDef, IntHp, MapContext, MapSchema};

fn main() {
    let mut ctx = MapContext::default();

    ctx.insert::<Atk>(SRC, 12.0);
    ctx.insert::<Hp>(SRC, 90.0);
    ctx.insert::<IntHp>(SRC, 200);
    ctx.insert::<IntDef>(SRC, 40);

    // Comparisons
    println!("{}", IntHp::get(SRC).gt(100).eval(&ctx).unwrap());
    println!("{}", IntHp::get(SRC).le(200).eval(&ctx).unwrap());
    println!("{}", IntHp::get(SRC).eq(200).eval(&ctx).unwrap());
    println!("{}", IntHp::get(SRC).ne(300).eval(&ctx).unwrap());

    // Bool composition
    let t = BoolExpr::<MapSchema>::true_();
    let f = BoolExpr::false_();
    println!("{}", (t.clone() & f.clone()).eval(&ctx).unwrap());
    println!("{}", (t.clone() | f.clone()).eval(&ctx).unwrap());
    println!("{}", (t.clone() ^ t.clone()).eval(&ctx).unwrap());
    println!("{}", (!t.clone()).eval(&ctx).unwrap());
    println!("{}", t.clone().nand(f.clone()).eval(&ctx).unwrap());
    println!("{}", t.clone().nor(f.clone()).eval(&ctx).unwrap());
    println!("{}", t.clone().xnor(f.clone()).eval(&ctx).unwrap());

    // then / otherwise
    let branch = Atk::get(SRC).gt(10.0).then(1.0).otherwise(0.0);
    println!("{}", branch.eval(&ctx).unwrap());

    // Cross-type: integer -> float
    let branch2 = IntDef::get(SRC).gt(30).if_then_else(50.0, 0.0);
    println!("{}", branch2.eval(&ctx).unwrap());
}
