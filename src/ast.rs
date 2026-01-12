// Typy operatorów matematycznych
#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
}

// Drzewo Składniowe (Abstract Syntax Tree)
#[derive(Debug, Clone)]
pub enum Expr {
    // Zwykła liczba, np. 3.14
    Number(f64),
    // Operacja dwuargumentowa: lewa strona, operator, prawa strona
    Binary(Box<Expr>, Op, Box<Expr>),
    // Operacja jednoargumentowa, np. minus przed liczbą: -5
    Unary(Box<Expr>), 
}