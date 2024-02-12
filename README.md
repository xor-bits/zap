# Zap

a programming language for (game) scripting

strongly typed, type inference, scripting language, interop, LLVM JIT

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
