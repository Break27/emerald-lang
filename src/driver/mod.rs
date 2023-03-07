use std::io::Write;
use inkwell::context::Context;
use inkwell::values::AsValueRef;
use crate::compiler::codegen::IRBuilder;
use crate::compiler::Compiler;
use crate::parser::config::{default_parser_config, ParserConfiguration};
use crate::parser::lexer::tokenize;
use crate::parser::parse;

pub fn run() {
    let mut config = default_parser_config();

    let mut ast = Vec::new();
    let mut prev = Vec::new();

    let context = Context::create();
    let builder = context.create_builder();
    let module = context.create_module("main");

    let mut compiler = Compiler::new(&context, &builder, &module);

    loop {
        if prev.is_empty() { print!("iem> ") } else { print!("...> ") }
        std::io::stdout().flush().unwrap();

        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();

        let tokens = match tokenize(input.as_str()) {
            Ok(tokens) => tokens,
            Err(e) => {
                return println!("Error: {}", e);
            }
        };

        prev.extend(tokens.into_iter());

        match parse(prev.as_slice(), ast.as_slice(), &mut config) {

            Ok((result, rest)) => {
                ast.extend(result.into_iter());

                if ! rest.is_empty() {
                    prev = rest;
                    continue
                }
                prev.clear();
            },
            Err((e, _)) => {
                println!("Error: {}", e);
                continue
            }
        }

        match ast.codegen(&&mut compiler) {
            Ok(result) => println!("{:?}", result.as_value_ref()),
            Err(e) => println!("Error: {}", e)
        };

        ast.clear();
    }
}
