use std::process::exit;

use codegen::{CodeGen, Str};
use lexer::Lexer;
use parser::{ast, Parse, ParseStream};

//

fn main() {
    let str = r#"  
        main := fn() -> i32 {
            print("Hello, world!");
            return 0;
        }
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

    let mut ast = ast.unwrap_or_else(|err| {
        eprintln!("parse err: {err}");
        exit(-1);
    });

    let mut codegen = CodeGen::new();
    let mut module = codegen.module();
    module
        .add_extern("print", print as extern "C" fn(Str))
        .unwrap();
    module
        .add_extern("sum", sum as extern "C" fn(i32, i32) -> i32)
        .unwrap();
    module.add(ast).unwrap();
    let val = module.run().unwrap();

    extern "C" fn print(s: Str) {
        println!("called print `{}`", s.as_str());
    }

    extern "C" fn sum(a: i32, b: i32) -> i32 {
        println!("called sum");
        a + b
    }

    println!("\nmain returned: `{val}`");
}
