#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    Add, Sub, Mul, Div, Pow
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Number(f64),
    Variable(String),
    ListGet(String, Box<Expr>),
    Binary(Box<Expr>, Op, Box<Expr>),
    Unary(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Assignment(String, Expr),
    ListAssignment(String, Expr, Expr),
    Block(Vec<Stmt>),
    If { cond: Expr, then_body: Box<Stmt>, else_body: Option<Box<Stmt>> },
    For { var: String, start: Expr, end: Expr, down: bool, body: Box<Stmt> },
    Return(Expr),
    Expr(Expr),
}