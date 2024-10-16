use std::{
    env::args,
    error::Error,
    fs::read_to_string,
    io::{stdin, stdout, Write},
    thread::{self, Thread},
    time::Duration,
};

use compiler::{Compiler, Str};
// use interpreter::Interpreter;
use bytecode::Runtime;
use lexer::Lexer;
use parser::{
    ast::{Ast, Root},
    ParseStream,
};
use typeck::Type;

//

fn main() -> Result<(), Box<dyn Error>> {
    // let mut compiler = Interpreter::new();
    let mut compiler = Compiler::new();
    // let mut compiler = Runtime::new();

    // compiler.add_extern("printi", Type::Void, &[Type::Str]);
    compiler.add("printi", |i: i32| println!("{i}")).unwrap();
    compiler.add("prints", |s: Str| println!("{s}")).unwrap();
    compiler
        .add("wait", || thread::sleep(Duration::from_millis(200)))
        .unwrap();
    compiler
        .run(
            "
            a := 0;
            b := 1;
            for {
                printi(a);
                a, b = b, a + b;
                wait();
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
