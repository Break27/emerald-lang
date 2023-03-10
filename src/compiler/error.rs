use custom_error::custom_error;

custom_error! { pub IRBuildError
    EmptyASTError = "empty AST",
    VariableNotFoundError { name: String } = "Variable '{name}' does not exist in scope",
    FunctionNotFoundError { name: String } = "Function '{name}' does not exist in scope",
    NotCallableError = "",
    InvalidEffectError = "",
    IncorrectArgumentListError { required: usize, provided: usize } = "",
    InvalidFunctionCallError = "Invalid function call",
    InvalidAssignmentOperationError = "Invalid assignment operation",
}
