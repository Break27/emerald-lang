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

type ParsingResult = Result<(Vec<ASTNode>, Vec<Token>), (ParseError, Token)>;
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
    ($tokens:ident, $parsed_tokens:ident, $body:tt) => {{
        let token = $tokens.last().map(|i| { i.clone() });

        expect!($tokens, $parsed_tokens, token, true, $body)
    }};

    ($tokens:ident, $parsed_tokens:ident, $current_token:expr, $pop_token:expr, {
        $($pattern:pat $(if $cond:expr)? => $result:stmt,)+ $(? => $not_matched:stmt)?
    }) => {
        match $current_token.clone() {
            $(
                Some($pattern) $(if $cond)? => {
                    let token = if $pop_token {
                        $tokens.pop()
                    } else {
                        $current_token
                    };
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

/// Expect and eat. This macro eats whatever token it meets,
/// regardless of whether it is a match or not.
macro_rules! feed_expect {
    ($tokens:ident, $parsed_tokens:ident, $body:tt) => {{
        let token = $tokens.pop();

        if token.is_none() {
            $parsed_tokens.reverse();
            $tokens.extend($parsed_tokens.into_iter());
            return Incomplete;
        }

        expect!($tokens, $parsed_tokens, token, false, $body)
    }}
}

/// expect and parse an enclosure.
macro_rules! enclosure {
    ($tokens:ident, $config:ident, $list:expr, $parsed_tokens:ident, {
        $closing:tt $(|$any:tt)? => $expression:ident($($args:ident),*), $delimiter:tt => continue
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
                    $delimiter => continue,
                    $( ? => { stringify!($any); break } )?
                });
            }

            let expr = parse! {
                $parsed_tokens;
                parse_base($tokens, $config)
            };
            list.push(expr);
        }

        $expression($($args,)*list)
    }}
}

macro_rules! list {
    ($tokens:ident, $config:ident, $body:tt) => {{
        let token = $tokens.pop().unwrap();
        let mut parsed_tokens = vec![token];

        return Success(enclosure!($tokens, $config, vec![], parsed_tokens, $body), parsed_tokens)
    }};
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
            Some(Delimiter) => break,
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
    Ok((ast, stack))
}

fn parse_base(tokens: &mut Vec<Token>, config: &mut Config) -> ExpressionResult {
    let mut parsed_tokens = Vec::new();

    let expr = parse! {
        parsed_tokens, tokens, config;
        parse_primary_expr();
        parse_binary_expr(0);

        parse_block_expr();
        parse_eval_expr()
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
            // block expression {}
            OpeningCurlyBrace => {
                let args = vec![];
                list!(tokens, config, {
                    ClosingCurlyBrace => BlockExpr(args),
                    Delimiter => continue
                })
            },
            token => {
                Failure(UnexpectedTokenError {
                    token: format!("{:?}", token)
                })
            }
        }
        None => Incomplete,
    }
}

fn parse_eval_expr(tokens: &mut Vec<Token>, config: &mut Config, lhs: Expression) -> ExpressionResult {
    let mut parsed_tokens = Vec::new();

    let result = match lhs {
        EvalExpr(name) => {

            let args = match tokens.last() {
                Some(Delimiter
                     | Comma
                     | ClosingParenthesis
                     | ClosingBracket
                     | ClosingCurlyBrace) => GroupExpr(vec![]),
                _ => parse! {
                    parsed_tokens, tokens, config;
                    parse_primary_expr();
                    parse_group_expr()
                }
            };

            // catch and parse potential access operation
            let call = CallExpr(name, box args);
            parse! {
                parsed_tokens;
                parse_binary_expr(tokens, config, 0, call)
            }
        },
        _ => lhs
    };

    Success(result, parsed_tokens)
}

fn parse_group_expr(tokens: &mut Vec<Token>, config: &mut Config, lhs: Expression) -> ExpressionResult {
    let mut parsed_tokens = Vec::new();

    let result = match tokens.last() {
        Some(Comma) => {
            enclosure!(tokens, config, vec![lhs], parsed_tokens, {
                Delimiter | _ => GroupExpr(),
                Comma => continue
            })
        },
        _ => lhs
    };

    Success(result, parsed_tokens)
}

fn parse_block_expr(tokens: &mut Vec<Token>, config: &mut Config, lhs: Expression) -> ExpressionResult {
    let mut parsed_tokens = Vec::new();

    let result = match tokens.last() {
        Some(OpeningCurlyBrace) => match lhs {

            GroupExpr(group) => {
                enclosure!(tokens, config, vec![], parsed_tokens, {
                    Delimiter | _ => BlockExpr(group),
                    Comma => continue
                })
            },
            _ => lhs
        }
        _ => lhs
    };

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

                    let name = match lhs {
                        EvalExpr(ref name) => name,
                        _ => return Failure(InvalidAssignmentOperation)
                    };

                    let expr = parse! {
                        parsed_tokens;
                        parse_base(tokens, config)
                    };

                    result = AssignExpr(name.to_string(), box expr);
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

        // merge LHS and RHS
        let call = CallExpr(operator, box rhs);
        result = AccessExpr(box result, box call)
    }

    Success(result, parsed_tokens)
}

fn parse_expression(tokens: &mut Vec<Token>, config: &mut Config) -> SnippetParsingResult<ASTNode> {
    let mut parsed_tokens = Vec::new();

    let effect = parse! {
        parsed_tokens, tokens, config;
        parse_base();
        parse_group_expr()
    };

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
