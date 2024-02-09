use compiler::{Compiler, Str};

//

fn main() {
    let str = r#"  
        main := fn() -> i32 {
            print("Hello, world!");
            sum(32, 32)
        }
    "#;

    let mut compiler = Compiler::new();

    compiler.add("sum", |a: i32, b: i32| a + b).unwrap();
    compiler.add("print", |a: Str| println!("{a}")).unwrap();

    let res = compiler.run(str).unwrap();
    println!("main returned: `{res}`");
}
