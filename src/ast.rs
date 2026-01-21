#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    
    Add, Sub, Mul, Div, Mod, Pow,
    
    And, Or, Not,
    
    Eq, Neq, Lt, Gt, Lte, Gte, 
}

#[derive(Debug, Clone, PartialEq)]
pub enum CastType {
    Int,    
    Uint,   
    Float,  
}

#[derive(Debug, Clone, PartialEq)]
pub enum CastMode {
    Bits,       
    Decimal,    
}

#[derive(Debug, Clone, PartialEq)]
pub struct CastSpec {
    pub cast_type: CastType,
    pub size: u32,       
    pub mode: CastMode,  
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Number(f64),
    Bool(bool),
    Variable(String),
    Binary(Box<Expr>, Op, Box<Expr>),
    Unary(Op, Box<Expr>),
    ListGet(String, Box<Expr>),
    

    Cast(CastSpec, Box<Expr>),

    ListLiteral(Vec<Expr>), 

    Call(Box<Expr>, Vec<Expr>), 
    Lambda(Vec<String>, Box<Stmt>), 
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Assignment(String, Expr),
    ListAssignment(String, Expr, Expr),
    Block(Vec<Stmt>),
    
    If {
        cond: Expr,
        then_body: Box<Stmt>,
        else_body: Option<Box<Stmt>>,
    },
    For {
        var: String,
        start: Expr,
        end: Expr,
        down: bool,
        body: Box<Stmt>,
    },
    While {
        cond: Expr,
        body: Box<Stmt>,
    },
    
    Function(String, Vec<String>, Box<Stmt>),

    Include(String),
    IncludeOnce(String),

    Return(Expr),
    Expr(Expr),
}