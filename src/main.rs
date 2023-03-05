use std::io::Write;
use emerald_lang::parser::config::default_parser_config;
use emerald_lang::parser::lexer::tokenize;
use emerald_lang::parser::parse;

fn main() {
    debug_parser()
}

fn debug_lexer() {
    loop {
        print!(">>> ");
        std::io::stdout().flush().expect("nothing.");
        let mut line = String::new();
        std::io::stdin().read_line(&mut line).unwrap();
        match tokenize(line.as_str()) {
            Ok(_) => continue,
            Err(e) => println!("{}", e)
        };
    }
}

fn debug_parser() {
    loop {
        print!(">>> ");
        std::io::stdout().flush().expect("nothing.");
        let mut line = String::new();
        std::io::stdin().read_line(&mut line).unwrap();

        match tokenize(line.as_str()) {
            Ok(tokens) => {
                let mut config = default_parser_config();

                match parse(&*tokens, &[], &mut config) {
                    Ok((result, _)) => println!("{:?}", result),
                    Err(e) => println!("{:?}", e)
                }
            },
            Err(e) => println!("{}", e)
        };
    }
}