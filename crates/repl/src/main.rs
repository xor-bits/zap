use std::{
    env::args,
    error::Error,
    fs::read_to_string,
    io::{stdin, stdout, Write},
    thread,
    time::Duration,
};

use compiler::{Compiler, Str};

//

fn main() -> Result<(), Box<dyn Error>> {
    let mut compiler = Compiler::new();

    compiler.add("sum", |a: i32, b: i32| a + b).unwrap();
    compiler.add("printb", |a: bool| println!("{a}")).unwrap();
    compiler.add("printi", |a: i32| println!("{a}")).unwrap();
    compiler.add("prints", |a: Str| println!("{a}")).unwrap();
    compiler
        .add("wait", || thread::sleep(Duration::from_millis(200)))
        .unwrap();

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
        0
    };

    if res != 0 {
        println!("main returned: `{res}`");
    }

    Ok(())
}
