use custom_error::custom_error;

custom_error! { pub LexError
    UnrecognizedTokenError { token: String } = "Unrecognized Token {token}",
    InvalidNumberError { token: String } = "Lexer failed at parsing number {token}",
    TooManyDotsError = "Too many dots",
}

custom_error! { pub ParseError
    UnexpectedTokenError { token: String } = "Unexpected token '{token}'.",
    InvalidAssignmentOperation = "",
    InvalidLiteralError = "Token provided is not a valid literal.",
    MissingExpectedTokenError { tokens: Vec<String> } = @{ format_missing_expected_token(tokens.clone(), "".to_string()) },
    UndefinedOperatorError { name: String } = "Unknown operator '{name}'.",
}

fn format_missing_expected_token(tokens: Vec<String>, message: String) -> String {
    let mut tokens = tokens;
    message +
        &*match tokens.len() {
            0 => return "".to_string(),
            2 => format!("'{}' or '{}'", tokens[0], tokens[1]),
            _ => format_missing_expected_token(
                tokens.clone(), format!("'{}', ", tokens.pop().unwrap())
            )
        } +
    " expected."
}
