# Zap

a programming language for (game) scripting

strongly typed, type inference, scripting language, interop, LLVM JIT

## Hello world + Fibonacci sequence

```go
prints("Fibonacci sequence:");
a := 0;
b := 1;
for {
    printi(a);
    a, b = b, a + b;
}
```

## Usage from Rust

```rust
let mut compiler = Compiler::new();
compiler.add("sum", |a: i32, b: i32| a + b).unwrap();
compiler.add("print", |a: i32| println!("{a}")).unwrap();
compiler.run(r#"
    print(sum(40, 2));
"#).unwrap();

```
