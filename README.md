# Zap

a programming language for (game) scripting

strongly typed, type inference, scripting language, interop, LLVM JIT

## Hello world + Fibonacci sequence

```go
fib := fn() {
    a := 0;
    b := 1;
    for {
        // TODO: f-strings
        print(f"{a}");

        tmp := a + b;
        a = b;
        b = tmp;
        // TODO: a, b = b, a + b;
    };
};

str := "Hello, world!";

main := () -> i32 {
    print(str);
    fib();
    return 0;
};
```

## Usage from Rust

```rust
let mut compiler = Compiler::new();
compiler.add("sum", |a: i32, b: i32| a + b).unwrap();
compiler.add("print", |a: i32| println!("{a}")).unwrap();
compiler.run(r#"
    main := () -> i32 {
        print(sum(40, 2));
        0
    }
"#).unwrap();

```

## TODO

the global scope should be the main function, maybe?

so this:
```go
s := "test";
print(s);
```

instead of:
```go
s := "test";
main := () -> i32 {
    print(s);
    return 0;
}
```
