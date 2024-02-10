use std::{thread, time::Duration};

use compiler::{Compiler, Str};

//

fn main() {
    let str = r#"  
        // recurse := fn(a: i32) {
        //     printi(a);
        //     wait();
        //     recurse(a + 1);
        // };

        // fib := fn() {
        //     a := 0;
        //     b := 1;
        //     for {
        //         printi(a);
        //         wait();

        //         tmp := a + b;
        //         a = b;
        //         b = tmp;
        //     };
        // };

        fizzbuzz := fn() {
            i := 1;
            for {
                f := i % 3 == 0;
                b := i % 5 == 0;
                if f && b {
                    prints("fizzbuzz");
                } else if f {
                    prints("fizz");
                } else if b {
                    prints("buzz");
                } else {
                    printi(i);
                };
                wait();
                i = i + 1;
            };
        };
    
        main := fn() -> i32 {
            // fib();
            // recurse(1);
            fizzbuzz();
            0
            // a := 1;
            // for {
            //     printi(a);
            //     a = a + 1;
            //     prints("waiting ..");
            //     wait();
            // };
            // sum(32, 32)
        }
    "#;

    let mut compiler = Compiler::new();

    compiler.add("sum", |a: i32, b: i32| a + b).unwrap();
    compiler.add("printb", |a: bool| println!("{a}")).unwrap();
    compiler.add("printi", |a: i32| println!("{a}")).unwrap();
    compiler.add("prints", |a: Str| println!("{a}")).unwrap();
    compiler
        .add("wait", || thread::sleep(Duration::from_millis(200)))
        .unwrap();

    let res = compiler.run(str).unwrap();
    println!("main returned: `{res}`");
}
