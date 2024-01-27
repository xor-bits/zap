use std::process::exit;

use interpreter::Runner;
use lexer::Lexer;
use parser::{ast, Parse, ParseStream};

//

fn main() {
    let str = r#"
        const := { 1 + 2 * 3 };

        add := fn(a: i32, b: i32) -> i32 {
            a + b
        };

        fn_factory := fn() -> i32 {
            fn(a: i32, b: i32) -> i32 {
                a + b
            }
        };

        main := fn() -> i32 {
            print("Hello world ", add(const, const));
            print("const = " + const);
            print("str con" + "cat");

            tmp := fn_factory();

            print("tmp: " + tmp(5, 6));
    
            return 0;
        };
    "#;
    // println!("{str}");

    let mut stream = ParseStream::from_lexer(Lexer::new(str));
    let ast: Result<ast::Ast<ast::Root>, _> =
        Parse::parse(&mut stream).map_err(|err| err.to_string());
    // let extra = stream
    //     .next()
    //     .transpose()
    //     .map(|t| t.map(|t| t.as_str().to_string()))
    //     .map_err(|err| err.to_string());
    // println!("{extra:?}");

    let ast = ast.unwrap_or_else(|err| {
        eprintln!("parse err: {err}");
        exit(-1);
    });

    let mut runner = Runner::new();
    runner.add_defaults();

    let val = runner.exec(&ast).unwrap_or_else(|err| {
        eprintln!("runtime err: {err:?}");
        exit(-1);
    });

    println!("\nmain returned: `{val}`");
}
