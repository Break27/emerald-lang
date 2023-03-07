use custom_error::custom_error;

custom_error! { pub IRBuildError
    EmptyASTError = "empty AST",
    ParserError { message: String } = "ParserError: {message}",
    VariableNotFound { name: String } = "Variable {name} does not exist in scope",
    FunctionNotFound { name: String } = "Function {name} does not exist in scope",
    IncorrectArgumentList { required: usize, provided: usize } = "",
    InvalidFunctionCall = "Invalid function call",
    InvalidAssignmentOperationError = "Invalid assignment operation",
}
