use SnippetParsingResult::*;
use crate::parser::error::ParseError;
use crate::parser::ast::{ASTNode, Expression, Expression::*};
use crate::parser::ast::ASTNode::EffectNode;
use crate::parser::error::ParseError::*;
use crate::parser::lexer::Token;
use crate::parser::lexer::Token::*;
use crate::parser::config::ParserConfiguration as Config;

pub mod ast;
pub mod lexer;
pub mod error;
pub mod config;

pub type ParsingResult = Result<(Vec<ASTNode>, Vec<Token>), (ParseError, Token)>;
type ExpressionResult = SnippetParsingResult<Expression>;

pub enum SnippetParsingResult<T> {
    Success(T, Vec<Token>),
    Incomplete,
    Failure(ParseError)
}

/// try parsing tokens
macro_rules! parse {
    ($parsed_tokens:ident, $tokens:expr, $config:ident; $primary:ident(); $( $function:ident($($arg:expr),*) );+) => {{
        let mut expr = parse! {
            $parsed_tokens;
            $primary($tokens, $config)
        };
        $(
            expr = parse! {
                $parsed_tokens;
                $function($tokens, $config $(,$arg)*, expr)
            };
        )+
        expr
    }};

    ($parsed_tokens:ident; $function:ident($tokens:expr, $config:ident $(,$arg:expr)*)) => (
        match $function($tokens, $config, $($arg),*) {
            Success(ast, _tokens) => {
                $parsed_tokens.extend(_tokens.into_iter());
                ast
            },
            Incomplete => {
                $parsed_tokens.reverse();
                $tokens.extend($parsed_tokens.into_iter());
                return Incomplete
            },
            Failure(e) => return Failure(e)
        }
    );
}

/// Expect for a token. If match, eat the token and push it to the parsed tokens.
/// Otherwise, execute $not_matched statement / block, or return parsing failure
/// by default.
macro_rules! expect {
    ($tokens:ident, $parsed_tokens:ident, {
        $($pattern:pat $(if $cond:expr)? => $result:stmt,)+ $(? => $not_matched:stmt)?
    }) => {
        match $tokens.last().map(|i| { i.clone() }) {
            $(
                Some($pattern) $(if $cond)? => {
                    let token = $tokens.pop();
                    $parsed_tokens.push(token.unwrap());
                    $result
                },
            )+
            $(
                _ => { $not_matched }
            )?
            #[allow(unreachable_patterns)]
            _ => return Failure(MissingExpectedTokenError {
                tokens: vec![$( stringify!($pattern).to_string() ),+]
            })
        }
    }
}

/// expect and parse an enclosure.
macro_rules! enclosure {
    ($list:expr, $parsed_tokens:ident, $tokens:ident, {
        $closing:tt $(|$any:tt)? => $expression:ident($($args:ident),*) as $parse:tt, $($delimiter:tt)|+ => continue
    }) => {{
        let mut list = $list;
        let mut index = list.len();
        loop {
            index += 1;

            expect!($tokens, $parsed_tokens, {
                $closing => break,
                ? => ()
            });

            if index % 2 == 0 {
                expect!($tokens, $parsed_tokens, {
                    $( $delimiter => continue, )+
                    $( ? => { stringify!($any); break } )?
                });
            }

            let expr = parse! $parse;
            list.push(expr);
        }

        $expression($($args,)*list)
    }}
}

macro_rules! list {
    ($tokens:ident, $config:ident, {
        $closing:tt $(|$any:tt)? => $expression:ident($($args:ident),*), $($delimiter:tt)|+ => continue
    }) => {{
        let token = $tokens.pop().unwrap();
        let mut parsed_tokens = vec![token];

        let expr = enclosure!(vec![], parsed_tokens, $tokens, {

            $closing $(|$any)? => $expression($($args),*) as {
                parsed_tokens, $tokens, $config;
                parse_primary_expr();
                parse_binary_expr(0);

                parse_eval_expr(false);
                parse_block_expr()
            },
            $($delimiter)|+ => continue
        });

        return Success(expr, parsed_tokens)
    }};
}

macro_rules! eat_all {
    ($tokens:expr, { $($token:pat_param)|+ }) => (
        loop { match $tokens.last() {
            // eat delimiter and semicolons
            Some($( $token )|+) => {
                $tokens.pop().unwrap();
            }
            _ => break
        }}
    );
}

pub fn parse(tokens: &[Token], parsed_tree: &[ASTNode], config: &mut Config) -> ParsingResult {
    // we read tokens from the end of the vector
    // using it as a stack
    let mut stack = tokens.to_vec();
    stack.reverse();

    let mut ast = parsed_tree.to_vec();

    loop {
        // look at the current token and determine what to parse
        // based on its value
        let token = match stack.last() {
            Some(Delimiter) => { stack.pop(); break }
            Some(token) => token.clone(),
            _ => break
        };
        match parse_expression(&mut stack, config) {
            Success(node, _) => ast.push(node),
            Incomplete => break,
            Failure(e) => return Err((e, token))
        }
    }

    stack.reverse();
    // consume all delimiters at the end of line
    eat_all!(stack, { Delimiter });
    Ok((ast, stack))
}

fn parse_base(tokens: &mut Vec<Token>, config: &mut Config) -> ExpressionResult {
    let mut parsed_tokens = Vec::new();

    let expr = parse! {
        parsed_tokens, tokens, config;
        parse_primary_expr();
        parse_binary_expr(0);

        parse_eval_expr(true);
        parse_block_expr()
    };

    Success(expr, parsed_tokens)
}

fn parse_primary_expr(tokens: &mut Vec<Token>, config: &mut Config) -> ExpressionResult {
    match tokens.last() {
        Some(token) => match token {
            Identifier(_) | Operator(_) => {
                parse_ident_expr(tokens, config)
            },
            Integer(_) | Float(_) => {
                parse_number_literal_expr(tokens, config)
            },
            StringLiteral(_) | FmtStringLiteral(_) | RegexLiteral(_) => {
                parse_string_literal_expr(tokens, config)
            },
            Do | OpeningCurlyBrace => {
                parse_block_expr(tokens, config, GroupExpr(vec![]))
            }
            // parenthesis expression ()
            OpeningParenthesis => {
                list!(tokens, config, {
                    ClosingParenthesis => GroupExpr(),
                    Comma => continue
                })
            },
            // list expression []
            OpeningBracket => {
                list!(tokens, config, {
                    ClosingBracket => ListExpr(),
                    Comma => continue
                })
            },
            Delimiter => Incomplete,

            token => {
                Failure(UnexpectedTokenError {
                    token: format!("{:?}", token)
                })
            }
        },
        None => Incomplete,
    }
}

fn parse_eval_expr(tokens: &mut Vec<Token>, config: &mut Config, explicit_call: bool, lhs: Expression) -> ExpressionResult {
    let mut parsed_tokens = Vec::new();
    let mut result = lhs;

    if let EvalExpr(name) = result.clone() {

        let expr = match tokens.last() {

            Some(OpeningParenthesis) => parse! {
                parsed_tokens;
                parse_primary_expr(tokens, config)
            },
            Some(Comma) => {
                return Success(result, parsed_tokens)
            },

            // in the inside of these expressions, function will not be called
            // explicitly without explicit_call enabled
            Some(Delimiter
                 | Semicolon
                 | ClosingParenthesis // of a group
                 | ClosingBracket // of a list
                 | ClosingCurlyBrace // of a block
            ) if explicit_call => GroupExpr(vec![]),

            // with explicit_call being true, EvalExpr will be parsed
            // into a call expression, even without parentheses
            _ if explicit_call => parse! {
                parsed_tokens, tokens, config;
                parse_primary_expr();
                parse_opengroup(true)
            },

            _ => return Success(result, parsed_tokens),
        };

        // convert a group or a single argument into a list
        let args = match expr {
            GroupExpr(group) => group,
            expr => vec![expr]
        };

        let call = CallExpr(name, args);

        // catch and parse potential access operation
        result = parse! {
            parsed_tokens;
            parse_binary_expr(tokens, config, 0, call)
        };
    };

    Success(result, parsed_tokens)
}

fn parse_opengroup(tokens: &mut Vec<Token>, config: &mut Config, eval: bool, lhs: Expression) -> ExpressionResult {
    let mut parsed_tokens = Vec::new();

    let mut result = match tokens.last() {
        Some(Comma) => if eval {
            enclosure!(vec![lhs], parsed_tokens, tokens, {
                Delimiter | _ => GroupExpr() as {
                    parsed_tokens, tokens, config;
                    parse_primary_expr();
                    parse_binary_expr(0);

                    parse_eval_expr(false);
                    parse_block_expr()
                },
                Comma => continue
            })
        } else {
            enclosure!(vec![lhs], parsed_tokens, tokens, {
                Delimiter | _ => GroupExpr() as {
                    parsed_tokens;
                    parse_primary_expr(tokens, config)
                },
                Comma => continue
            })
        },
        _ => lhs
    };

    if ! eval { if let Some(Assignment) = tokens.last() {
        // parse potential assignment operation
        result = parse! {
            parsed_tokens;
            parse_binary_expr(tokens, config, 0, result)
        }
    }}

    Success(result, parsed_tokens)
}

fn parse_block_expr(tokens: &mut Vec<Token>, config: &mut Config, lhs: Expression) -> ExpressionResult {
    let mut parsed_tokens = Vec::new();

    let mut forced = false;
    let mut result = lhs;

    expect!(tokens, parsed_tokens, {
        Do => if let Some(OpeningParenthesis) = tokens.last() {
            result = parse! {
                parsed_tokens;
                parse_primary_expr(tokens, config)
            };
            forced = true;
        },
        ? => ()
    });

    if let GroupExpr(args) = result.clone() {
        result = match tokens.last() {
            Some(OpeningCurlyBrace) => {
                // eat opening token
                let token = tokens.pop().unwrap();
                parsed_tokens.push(token);

                enclosure!(vec![], parsed_tokens, tokens, {
                    ClosingCurlyBrace => BlockExpr(args) as {
                        parsed_tokens;
                        parse_base(tokens, config)
                    },
                    Delimiter | Semicolon => continue
                })
            },
            _ if forced => return Failure(MalformedDoExpression),
            _ => result
        };
    }

    Success(result, parsed_tokens)
}

fn parse_binary_expr(tokens: &mut Vec<Token>, config: &mut Config, precedence: i32, lhs: Expression) -> ExpressionResult {
    // start with LHS value
    let mut result = lhs.clone();
    let mut parsed_tokens = Vec::new();

    macro_rules! match_operator {
        ($pattern:pat if $cond:expr => $matched:expr) => (
            match tokens.last().map(|i| i.clone()) {

                Some(Operator(ref name)) => {
                    match config.operators.get(name) {
                        $pattern if $cond => {
                            (name.to_string(), $matched)
                        },
                        None => {
                            return Failure(UndefinedOperatorError {
                                name: name.to_string()
                            })
                        },
                        _ => break
                    }
                },
                // parse access operation
                Some(Dot) => {
                    let token = tokens.pop().unwrap();
                    parsed_tokens.push(token);

                    match tokens.last() {
                        Some(Operator(_)) => continue,
                        _ => ()
                    }

                    let expr = parse! {
                        parsed_tokens;
                        parse_base(tokens, config)
                    };

                    result = AccessExpr(box result, box expr);
                    break
                },
                // parse assignment operation
                Some(Assignment) => {
                    let token = tokens.pop().unwrap();
                    parsed_tokens.push(token);

                    let list = match result {
                        GroupExpr(group) => group,
                        _ => vec![result]
                    };

                    let expr = parse! {
                        parsed_tokens;
                        parse_base(tokens, config)
                    };

                    result = AssignExpr(list, box expr);
                    break
                },
                _ => break
            }
        );
    }

    loop {
        // continue until the current token is not an operator
        // or it is an operator with precedence lesser than expr_precedence
        let (operator, precedence) = match_operator! {
            Some(op) if op.precedence >= precedence => op.precedence
        };

        let token = tokens.pop().unwrap();
        parsed_tokens.push(token);

        // parse primary RHS expression
        let mut rhs = parse! {
            parsed_tokens;
            parse_primary_expr(tokens, config)
        };

        // parse all the RHS operators until their precedence is
        // bigger than the current one
        loop {
            let (_, binary_rhs) = match_operator! {
                Some(op) if op.precedence > precedence => parse! {
                    parsed_tokens;
                    parse_binary_expr(tokens, config, op.precedence, rhs)
                }
            };
            rhs = binary_rhs;
        }

        let args = match rhs {
            GroupExpr(group) => group,
            expr => vec![expr]
        };

        // merge LHS and RHS
        let call = CallExpr(operator, args);
        result = AccessExpr(box result, box call)
    }

    Success(result, parsed_tokens)
}

fn parse_expression(tokens: &mut Vec<Token>, config: &mut Config) -> SnippetParsingResult<ASTNode> {
    let mut parsed_tokens = Vec::new();

    let effect = parse! {
        parsed_tokens, tokens, config;
        parse_base();
        parse_opengroup(false)
    };

    eat_all!(tokens, { Delimiter | Semicolon });

    Success(EffectNode(effect), parsed_tokens)
}

fn parse_ident_expr(tokens: &mut Vec<Token>, config: &mut Config) -> ExpressionResult {
    let mut parsed_tokens = Vec::new();

    let name = expect!(tokens, parsed_tokens, {
        Identifier(name) => name,
        Operator(op) => op,
    });

    Success(EvalExpr(name), parsed_tokens)
}

fn parse_number_literal_expr(tokens: &mut Vec<Token>, config: &mut Config) -> ExpressionResult {
    let mut parsed_tokens = Vec::new();

    let expr = expect!(tokens, parsed_tokens, {
        Integer(val) => IntegerLiteralExpr(val),
        Float(val) => FloatLiteralExpr(val),
    });

    Success(expr, parsed_tokens)
}

fn parse_string_literal_expr(tokens: &mut Vec<Token>, config: &mut Config) -> ExpressionResult {
    let mut parsed_tokens = Vec::new();

    let expr = expect!(tokens, parsed_tokens, {
        StringLiteral(val) => StringLiteralExpr(val),
    });

    Success(expr, parsed_tokens)
}
