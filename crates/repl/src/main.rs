use compiler::{Compiler, Str};

//

fn main() {
    let str = r#"  
        main := fn() -> i32 {
            print("Hello, world!");
            return 0;
        }
    "#;

    let mut compiler = Compiler::new();

    compiler
        .add_raw_extern("print", print as extern "C" fn(Str))
        .unwrap();
    compiler
        .add_raw_extern("sum", sum as extern "C" fn(i32, i32) -> i32)
        .unwrap();

    let res = compiler.run(str).unwrap();
    println!("main returned: `{res}`");
}

extern "C" fn print(s: Str) {
    println!("called print `{}`", s.as_str());
}

extern "C" fn sum(a: i32, b: i32) -> i32 {
    println!("called sum");
    a + b
}
