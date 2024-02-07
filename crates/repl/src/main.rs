use std::process::exit;

use lexer::Lexer;
use parser::{ast, Parse, ParseStream};
use typeck::{Context, TypeCheck};

//

fn main() {
    let str = r#"  
        // const := { 1 + 2 * 3 };

        add := fn(a: i32, b: i32) -> i32 {
            return a + b;
        }
    
        // main := fn() -> i32 {
        //     tmp := 4;
        //     sum := add(tmp, const);
        //     print();
        //     return sum;
        // }

        typeck_a := fn() -> i32 {
            return 4 * add(2, 5) - 2;
        }
        // should turn into this:
        // typeck_b := fn() -> i32 {
        //     _0: i32 := 4;
        //     _1: impl Fn(i32,i32)->i32 := add;
        //     _1: i32 := _1.call(2, 5);
        //     _2: i32 := tmp_0 * tmp_1;
        //     _3: i32 := 2;
        //     _4: i32 := tmp_2 - tmp_3;
        //     return tmp_4;
        // }

        // add := fn(a: i32, b: i32) -> i32 {
        //     a + b
        // };

        // fn_factory := fn() -> i32 {
        //     fn(a: i32, b: i32) -> i32 {
        //         a + b
        //     }
        // };

        // main := fn() -> i32 {
        //     print("Hello world ", add(const, const));
        //     print("const = " + const);
        //     print("str con" + "cat");

        //     tmp := fn_factory();

        //     print("tmp: " + tmp(5, 6));
    
        //     return 0;
        // };
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

    let mut ctx = Context::new();
    ast.type_check(&mut ctx);

    println!("{ctx}");
    // println!("{ast:#?}");

    // let mut ctx = Context::new();
    // ctx.add_built_in();
    // ast.analyze(&mut ctx);
    // println!("{ctx:#?}");

    // let mut runner = Runner::new();
    // runner.add_defaults();

    // let val = runner.exec(&ast).unwrap_or_else(|err| {
    //     eprintln!("runtime err: {err:?}");
    //     exit(-1);
    // });

    // let mut codegen = CodeGen::new();
    // let mut module = codegen.module();
    // module
    //     .add_extern("print", || {
    //         println!("called print");
    //     })
    //     .unwrap();
    // module.add(&ast).unwrap();
    // let val = module.run().unwrap();

    // println!("\nmain returned: `{val}`");
}
