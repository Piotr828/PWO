use crate::ast::{Expr, Op};
use crate::lexer::Token;

pub fn parse(tokens: Vec<Token>) -> Result<Expr, String> {
    let mut p = Parser { tokens, pos: 0 };
    p.parse_expression()
}

struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    // Poziom 1: Dodawanie i Odejmowanie (najniższy priorytet)
    fn parse_expression(&mut self) -> Result<Expr, String> {
        let mut lhs = self.parse_term()?;

        loop {
            let op = match self.peek() {
                Token::Plus => Op::Add,
                Token::Minus => Op::Sub,
                _ => break,
            };
            self.advance();
            let rhs = self.parse_term()?;
            lhs = Expr::Binary(Box::new(lhs), op, Box::new(rhs));
        }
        Ok(lhs)
    }

    // Poziom 2: Mnożenie i Dzielenie (wyższy priorytet)
    fn parse_term(&mut self) -> Result<Expr, String> {
        let mut lhs = self.parse_factor()?;

        loop {
            let op = match self.peek() {
                Token::Star => Op::Mul,
                Token::Slash => Op::Div,
                _ => break,
            };
            self.advance();
            let rhs = self.parse_factor()?;
            lhs = Expr::Binary(Box::new(lhs), op, Box::new(rhs));
        }
        Ok(lhs)
    }

    // Poziom 3: Liczby, Nawiasy i Unarny Minus (najwyższy priorytet)
// Poziom 3: Liczby, Nawiasy i Unarny Minus
    fn parse_factor(&mut self) -> Result<Expr, String> {
        // 1. Klonujemy token, żeby nie blokować 'self' w matchu
        let token = self.peek().clone(); 

        match token {
            Token::Number(n) => {
                self.advance();
                Ok(Expr::Number(n)) // Tu już nie musisz używać *n, bo n to kopia f64
            }
            Token::Minus => {
                self.advance();
                let expr = self.parse_factor()?;
                Ok(Expr::Unary(Box::new(expr)))
            }
            Token::LParen => {
                self.advance();
                let expr = self.parse_expression()?;
                // Tu musimy sprawdzić kolejny token, znów używamy klona lub peeka
                if let Token::RParen = self.peek() {
                    self.advance();
                    Ok(expr)
                } else {
                    Err("Oczekiwano zamykającego nawiasu ')'".into())
                }
            }
            Token::Eof => Err("Nieoczekiwany koniec wyrażenia".into()),
            t => Err(format!("Nieoczekiwany token: {:?}", t)),
        }
    }

    // Pomocnicze: podglądanie obecnego tokena
    fn peek(&self) -> &Token {
        self.tokens.get(self.pos).unwrap_or(&Token::Eof)
    }

    // Pomocnicze: przesunięcie o jeden token do przodu
    fn advance(&mut self) {
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
    }
}