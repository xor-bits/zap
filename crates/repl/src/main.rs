use std::{
    error::Error,
    thread::{self},
    time::{Duration, Instant},
};

use compiler::{Compiler, Str};

//

fn main() -> Result<(), Box<dyn Error>> {
    // let mut compiler = Interpreter::new();
    let mut compiler = Compiler::new();
    // let mut compiler = Runtime::new();

    let mut rng = BadRng::init();

    // compiler.add_extern("printi", Type::Void, &[Type::Str]);
    compiler.add("rand", || rng.next()).unwrap();
    compiler.add("print", |i: i32| println!("{i}")).unwrap();
    compiler.add("print", |s: Str| println!("{s}")).unwrap();
    compiler
        .add("wait", || thread::sleep(Duration::from_millis(200)))
        .unwrap();
    compiler
        .run(
            "            
            fib := fn() {
                print(\"Fibonacci sequence:\");
                a := 0;
                b := 1;
                for {
                    print(a);
                    a, b = b, a + b;
                    wait();

                    if a >= 100 {
                        return;
                    }
                }
            }

            fizzbuzz := fn() {
                print(\"FizzBuzz:\");
                i := 1;
                for {
                    if ((i % 3) == 0) && ((i % 5) == 0) {
                        print(\"FizzBuzz\");
                    } else if (i % 3) == 0 {
                        print(\"Fizz\");
                    } else if (i % 5) == 0 {
                        print(\"Buzz\");
                    } else {
                        print(i);
                    }

                    wait();
                    i = i + 1;
                }
            }

            print_randoms := fn() {
                print(\"Random numbers:\");
                i := 20;
                for {
                    if i == 0 {
                        return;
                    }
                    print(rand());
                    i = i - 1;
                }
            }

            v := rand() % 3;
            if v == 0 {
                fizzbuzz();
            } else if v == 1 {
                fib();
            } else {
                print_randoms();
            }
        ",
        )
        .unwrap();

    // println!("{module:#?}");

    Ok(())

    /* compiler
        .run(
            r"
        main := fn() {
            x := 4;
            printi(x);
        }
    ",
        )
        .unwrap();

    // compiler.add("sum", |a: i32, b: i32| a + b).unwrap();
    // compiler.add("printb", |a: bool| println!("{a}")).unwrap();
    // compiler.add("printi", |a: i32| println!("{a}")).unwrap();
    // compiler.add("prints", |a: Str| println!("{a}")).unwrap();
    // compiler
    //     .add("wait", || thread::sleep(Duration::from_millis(200)))
    //     .unwrap();

    let res = if let Some(path_arg) = args().nth(1) {
        let src = read_to_string(path_arg)?;
        compiler.run(src.as_str())?
    } else {
        let stdin = stdin();
        let mut buf = String::new();
        loop {
            print!(">>> ");
            stdout().flush()?;

            buf.clear();
            let line_len = stdin.read_line(&mut buf)?;
            if line_len == 0 {
                break;
            }
            let line = &buf[..line_len];

            if let Err(e) = compiler.run(line) {
                eprintln!("{e}");
            }

            println!();
        }
    };

    // if res != 0 {
    //     println!("main returned: `{res:?}`");
    // }

    Ok(()) */
}

//

/// xor shift rng seeded with subsec nanos elapsed from init to first use
struct BadRng {
    init: Instant,
    state: Option<i32>,
}

impl BadRng {
    fn init() -> Self {
        Self {
            init: Instant::now(),
            state: None,
        }
    }

    fn next(&mut self) -> i32 {
        let state = self
            .state
            .get_or_insert_with(|| self.init.elapsed().subsec_nanos() as i32);
        let mut x = *state;
        x ^= x << 13;
        x ^= x >> 17;
        x ^= x << 5;
        *state = x;
        x
    }
}
