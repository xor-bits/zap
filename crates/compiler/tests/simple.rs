use std::{
    fmt::Write,
    process::exit,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Mutex,
    },
};

use codegen::Str;
use compiler::Compiler;

//

#[test]
fn fizz_buzz() {
    let source = r#"
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
                if i >= 5 {
                    return;
                };
                wait();
                i = i + 1;
            };
        };

        main := fn() -> i32 {
            fizzbuzz();
            return 0;
        }
    "#;

    let mut compiler = Compiler::new();

    static BUF: Mutex<String> = Mutex::new(String::new());
    static WAIT_COUNT: AtomicUsize = AtomicUsize::new(0);

    compiler
        .add("prints", |s: Str| {
            BUF.lock()
                .unwrap()
                .write_fmt(format_args!("{s}\n"))
                .unwrap();
        })
        .unwrap();
    compiler
        .add("printi", |i: i32| {
            BUF.lock()
                .unwrap()
                .write_fmt(format_args!("{i}\n"))
                .unwrap();
        })
        .unwrap();
    compiler
        .add("wait", || {
            if WAIT_COUNT.fetch_add(1, Ordering::Relaxed) == 20 {
                // enough fizzbuzzing

                let buf = BUF.lock().unwrap();
                assert_eq!(buf.as_str(), "1\n2\nfizz\n4\nbuzz\nfizz\n7\n8\nfizz\nbuzz\n11\nfizz\n13\n14\nfizzbuzz\n16\n17\nfizz\n19\nbuzz\nfizz\n");
                exit(0);
            }
        })
        .unwrap();

    compiler.run(source).unwrap();
}
