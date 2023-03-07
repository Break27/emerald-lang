#[derive(Debug, Clone)]
pub enum ASTNode {
    EffectNode(Expression)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    StringLiteralExpr(String),
    IntegerLiteralExpr(i32),
    FloatLiteralExpr(f64),
    BinaryExpr(String, Box<Expression>, Box<Expression>),
    ConditionalExpr {
        cond_expr: Box<Expression>,
        then_expr: Box<Expression>,
        else_expr: Box<Expression>
    },
    LoopExpr {
        item: String,
        iterable: Box<Expression>,
        step: Box<Expression>,
        body: Box<Expression>
    },
    EvalExpr(String),
    AssignExpr(Vec<Expression>, Box<Expression>),
    AccessExpr(Box<Expression>, Box<Expression>),
    CallExpr(String, Vec<Expression>),
    BlockExpr(Vec<Expression>, Vec<Expression>),
    GroupExpr(Vec<Expression>),
    ListExpr(Vec<Expression>),
    NoneExpr,
}
