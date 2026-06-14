use express_it::test_utils::scopes::{DST, SRC};
use express_it::test_utils::{Atk, Hp, MapContext};

fn main() {
    let mut ctx = MapContext::default();

    ctx.insert::<Atk>(SRC, 25.0);
    ctx.insert::<Hp>(SRC, 120.0);
    ctx.insert::<Hp>(DST, 150.0);

    // Basic lookup + arithmetic
    let hit = Hp::get(DST) - Atk::get(SRC);
    println!("hit = {}", hit.eval(&ctx).unwrap());

    let new = Hp::get(SRC) + Atk::get(SRC) * 0.1;
    println!("new = {}", new.eval(&ctx).unwrap());

    // Standard ops overload
    let a = Atk::get(SRC) + Atk::get(SRC);
    let b = Hp::get(SRC) - a.clone();
    let c = a.clone() * Hp::get(DST);
    let d = c.clone() / a.clone();
    let r = d.clone() % Hp::get(SRC);
    println!(
        "a = {}, b = {}, c = {}, d = {}, r = {}",
        a.eval(&ctx).unwrap(),
        b.eval(&ctx).unwrap(),
        c.eval(&ctx).unwrap(),
        d.eval(&ctx).unwrap(),
        r.eval(&ctx).unwrap()
    );

    // Extended ops
    let pw = Atk::get(SRC).pow(2.0);
    let mn = Hp::get(SRC).min(Hp::get(DST));
    let mx = Hp::get(SRC).max(Hp::get(DST));
    println!(
        "pow = {}, min = {}, max = {}",
        pw.eval(&ctx).unwrap(),
        mn.eval(&ctx).unwrap(),
        mx.eval(&ctx).unwrap()
    );
}
