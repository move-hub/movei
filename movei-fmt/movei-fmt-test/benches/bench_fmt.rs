use criterion::{criterion_group, criterion_main, Criterion};
use move_lang::parser;
use movei_fmt::{format, Formatter};
use std::time::Duration;

pub static LIBRA_SOURCE: &str = include_str!("./stdlib/Libra.move");
pub static ACCOUNT_LIMITS_SOURCE: &str = include_str!("./stdlib/AccountLimits.move");

fn bench_move_fmt(c: &mut Criterion) {
    let mut group = c.benchmark_group("move-fmt");
    for (name, source) in &[
        ("Libra", LIBRA_SOURCE),
        ("AccountLimits", ACCOUNT_LIMITS_SOURCE),
    ] {
        for width in &[80, 100, 120, 150] {
            let (stripped, mut comments, mut regular_comments) =
                move_lang::strip_comments_and_verify("bench", source).unwrap();
            let (defs, _comments) =
                parser::syntax::parse_file_string("test", stripped.as_str(), comments.clone())
                    .unwrap();
            let def = defs.first().unwrap();

            comments.append(&mut regular_comments);

            let formatter = Formatter::new(source, comments, 4);

            group
                .bench_with_input(
                    format!("{}-{}", name, width),
                    &(formatter, def),
                    |b, (formatter, def)| {
                        b.iter(|| {
                            let doc = formatter.definition(def);
                            format(*width, doc);
                        });
                    },
                )
                .measurement_time(Duration::from_secs(30));
        }
    }

    group.finish();
}

criterion_group!(benches, bench_move_fmt);
criterion_main!(benches);
