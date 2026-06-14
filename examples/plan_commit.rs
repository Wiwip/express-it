use express_it::frame::LazyPlan;
use express_it::test_utils::scopes::{DST, SRC};
use express_it::test_utils::{Atk, Def, Hp, MapContext};

fn main() {
    let mut ctx = MapContext::default();
    ctx.insert::<Atk>(SRC, 10.0);
    ctx.insert::<Def>(DST, 2.0);
    ctx.insert::<Hp>(DST, 20.0);

    let dmg = (Atk::get(SRC) - Def::get(DST)).max(0.0).alias("dmg");
    let new_hp = Hp::get(DST) - dmg.expr.clone();

    let plan = LazyPlan::new().step(dmg).step(Hp::set(DST, new_hp));

    plan.commit(&mut ctx).unwrap();

    let hp = Hp::get(DST);

    println!("hp = {}", hp.eval(&ctx).unwrap());
}
