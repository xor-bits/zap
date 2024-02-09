use std::{thread, time::Duration};

use compiler::{Compiler, Str};

//

fn main() {
    let str = r#"  
        main := fn() -> i32 {
            a := 1;
            for {
                printi(a);
                a = a + 1;
                prints("waiting ..");
                wait();
            };
            sum(32, 32)
        }
    "#;

    let mut compiler = Compiler::new();

    compiler.add("sum", |a: i32, b: i32| a + b).unwrap();
    compiler.add("printi", |a: i32| println!("{a}")).unwrap();
    compiler.add("prints", |a: Str| println!("{a}")).unwrap();
    compiler
        .add("wait", || thread::sleep(Duration::from_millis(200)))
        .unwrap();

    let res = compiler.run(str).unwrap();
    println!("main returned: `{res}`");
}
