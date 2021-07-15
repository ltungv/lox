use criterion::{black_box, criterion_group, criterion_main, Criterion};
use rlox::VM;

pub fn batched_10k_method_calls(c: &mut Criterion) {
    c.bench_function("batched_10k_method_calls", |b| {
        let mut vm = VM::default();
        let script = r#"
        class Foo { foo() {} }
        var foo = Foo();
        for (var i = 0; i < 10000; i = i + 1) { foo.foo(); }
        "#;
        b.iter(|| vm.interpret(black_box(script)));
    });
}

criterion_group!(benches, batched_10k_method_calls);
criterion_main!(benches);
