use express_it::test_utils::scopes::{DST, SRC};
use express_it::test_utils::{Atk, Hp, IntDef, IntHp, MapContext};
use std::ops::Neg;

fn main() {
    let mut ctx = MapContext::default();

    ctx.insert::<Hp>(SRC, 25.0);
    ctx.insert::<Atk>(SRC, -8.0);
    ctx.insert::<Hp>(DST, 10.0);

    ctx.insert::<IntHp>(SRC, 150);
    ctx.insert::<IntDef>(DST, -20);
    ctx.insert::<IntDef>(SRC, 1);

    println!("neg = {}", Hp::get(SRC).neg().eval(&ctx).unwrap());
    println!("sin = {}", Hp::get(SRC).sin().eval(&ctx).unwrap());
    println!("sqrt = {}", Hp::get(SRC).sqrt().eval(&ctx).unwrap());
    println!("floor = {}", Atk::get(SRC).floor().eval(&ctx).unwrap());
    println!("ceil = {}", Atk::get(SRC).ceil().eval(&ctx).unwrap());
    println!(
        "clamp = {}",
        Hp::get(SRC).clamp(Hp::get(DST), 100.0).eval(&ctx).unwrap()
    );

    println!("neg = {}", IntDef::get(DST).neg().eval(&ctx).unwrap());
    println!(
        "max = {}",
        IntDef::get(DST).max(IntDef::get(SRC)).eval(&ctx).unwrap()
    );
    println!(
        "clamp = {}",
        IntHp::get(SRC).clamp(0u32, 100u32).eval(&ctx).unwrap()
    );
    println!("pow = {}", IntHp::get(SRC).pow(0u32).eval(&ctx).unwrap());

    let casted = IntHp::get(SRC).as_::<f32>();
    println!("cast = {}", casted.eval(&ctx).unwrap());

    let as_f64 = Hp::get(SRC).as_::<f64>();
    println!("as f64 = {}", as_f64.eval(&ctx).unwrap());
}
