use std::ops::Deref;
use std::str::FromStr;
use regex::Captures;
use regex_macro::regex;
use crate::parser::error::LexError;
use crate::parser::lexer::Token::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Identifier(String),
    Operator(String),
    Atom(String),
    RegexLiteral(String),
    FmtStringLiteral(String),
    StringLiteral(String),
    Integer(i32),
    Float(f64),

    Do,
    Return,
    Begin,
    End,

    If,
    Elif,
    Else,
    For,
    While,

    Is,
    In,
    And,
    Or,
    Not,

    OpeningParenthesis,
    ClosingParenthesis,
    OpeningCurlyBrace,
    ClosingCurlyBrace,
    OpeningBracket,
    ClosingBracket,
    RangeInclusive,
    RangeExclusive,

    Dot,
    Comma,
    Delimiter,
    Apostrophe,
    Backquote,
    Semicolon,

    Assignment,
    Modify,
}

pub fn tokenize(input: &str) -> Result<Vec<Token>, LexError> {
    let comments = regex!(r"(?m)#.*\n");
    let code = comments.replace_all(input, "\n");

    let mut result = Vec::new();

    let token_regex = regex!(concat!(
        r"(?P<identifier>([\$~@%_]|\p{L}\d*)+)|",
        r"(?P<atom>:[\p{Alphabetic}_]\w*)|",
        r"(?P<float>(\d*\.\d+)+)|",
        r"(?P<integer>\d+)|",
        r"(?P<operator>[\+\-\*\^\|&=/<>?!:]+)|",
        r"(?P<enclosure>[\(\)\[\]\{\}])|",
        r"(?P<delimiter>[\n,;])|",
        r"(?P<dot>\.+)|",
        r"(?P<string>('[^']*')|",
         "(?P<fmtstring>(\"[^\"]*\")|`[^`]*`))|",
        r"(?P<regex>\\[^\\]*\\)|",
        r"(\S+)"
    ));

    for bits in token_regex.captures_iter(code.deref()) {
        let token = match_token(bits)?;

        result.push(token);
    }

    Ok(result)
}

fn match_token(bits: Captures) -> Result<Token, LexError> {
    let tokens_table: Vec<(&str, fn(&str) -> Result<Token, LexError>)> = vec![
        ("identifier", |token| Ok( match token {
            "do"     => Do,
            "return" => Return,
            "begin"  => Begin,
            "end"    => End,

            "if"     => If,
            "elif"   => Elif,
            "else"   => Else,
            "for"    => For,
            "while"  => While,

            "is"     => Is,
            "in"     => In,
            "or"     => Or,
            "and"    => And,
            "not"    => Not,

            ident => match ident {
                "none" | "true" | "false" => Atom(ident.to_string()),
                _ => Identifier(ident.to_string())
            }
        })),
        ("atom", |token| {
            Ok( Atom(token[1..].to_string()) )
        }),
        ("integer", |token| {
            parse_number(token, Integer)
        }),
        ("float", |token| {
            parse_number(token, Float)
        }),
        ("string", |token| {
            let pat: &[_] = &['"', '\'', '`'];
            Ok( StringLiteral(token.trim_matches(pat).to_string()) )
        }),
        ("regex", |token| {
            Ok( RegexLiteral(token.to_string()) )
        }),
        ("operator", |token| Ok( match token {
            "=" => Assignment,
             _  => Operator(token.to_string())
        })),
        ("enclosure", |token| Ok( match token {
            "(" => OpeningParenthesis,
            ")" => ClosingParenthesis,
            "{" => OpeningCurlyBrace,
            "}" => ClosingCurlyBrace,
            "[" => OpeningBracket,
             _  => ClosingBracket
        })),
        ("dot", |token| match token.len() {
            1 => Ok(Dot),
            2 => Ok(RangeExclusive),
            3 => Ok(RangeInclusive),
            _ => Err(LexError::TooManyDotsError)
        }),
        ("delimiter", |token| Ok( match token {
            "," => Comma,
            ";" => Semicolon,
             _  => Delimiter,
        }))
    ];

    fn parse_number<T: FromStr>(literal: &str, token: fn(T) -> Token) -> Result<Token, LexError> {
        match literal.parse() {
            Ok(number) => Ok(token(number)),
            Err(_) => Err(LexError::InvalidNumberError { token: literal.to_string() })
        }
    }

    for (name, effect) in tokens_table {
        //
        if bits.name(name).is_some() {
            let token = bits.name(name).unwrap().as_str();
            return effect(token);
        }
    }

    Err(LexError::UnrecognizedTokenError {
        token: bits[0].to_string()
    })
}
