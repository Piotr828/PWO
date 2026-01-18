use crate::ast::{Expr, Op, Stmt};
use crate::lexer::Token;
use chumsky::prelude::*;

type ParserInput<'src> = &'src [Token];
type ParserExtra<'src> = extra::Err<Rich<'src, Token>>;

pub fn parse(tokens: Vec<Token>) -> Result<Vec<Stmt>, String> {
    let input = tokens.as_slice();
    let parser = stmt_parser().repeated().collect()
        .then_ignore(just(Token::Eof).or_not())
        .then_ignore(end());

    parser.parse(input).into_result().map_err(|e| {
        e.into_iter().map(|err| format!("{:?}", err)).collect::<Vec<_>>().join("\n")
    })
}

fn expr_parser<'src>() -> impl Parser<'src, ParserInput<'src>, Expr, ParserExtra<'src>> + Clone {
    recursive(|expr| {
        let variable_or_list = select! { Token::Ident(id) => id }
            .then(expr.clone().delimited_by(just(Token::LBracket), just(Token::RBracket)).or_not())
            .map(|(id, idx)| match idx {
                Some(index) => Expr::ListGet(id, Box::new(index)),
                None => Expr::Variable(id),
            });

        let atom = choice((
            select! { Token::Number(n) => Expr::Number(n) },
            variable_or_list,
            expr.clone().delimited_by(just(Token::LParen), just(Token::RParen)),
        ));

        let op_pow = just(Token::Caret).to(Op::Pow);
        let power = atom.clone().then(op_pow.then(atom).repeated().collect::<Vec<_>>())
            .map(|(first, rest)| rest.into_iter().fold(first, |l, (op, r)| Expr::Binary(Box::new(l), op, Box::new(r))));

        let unary = choice((
            just(Token::Minus).ignore_then(power.clone()).map(|e| Expr::Unary(Box::new(e))),
            power,
        ));

        let op_mul = choice((just(Token::Star).to(Op::Mul), just(Token::Slash).to(Op::Div)));
        let product = unary.clone().then(op_mul.then(unary).repeated().collect::<Vec<_>>())
            .map(|(first, rest)| rest.into_iter().fold(first, |l, (op, r)| Expr::Binary(Box::new(l), op, Box::new(r))));

        let op_sum = choice((just(Token::Plus).to(Op::Add), just(Token::Minus).to(Op::Sub)));
        product.clone().then(op_sum.then(product).repeated().collect::<Vec<_>>())
            .map(|(first, rest)| rest.into_iter().fold(first, |l, (op, r)| Expr::Binary(Box::new(l), op, Box::new(r))))
    })
}

fn stmt_parser<'src>() -> impl Parser<'src, ParserInput<'src>, Stmt, ParserExtra<'src>> {
    recursive(|stmt| {
        let expr = expr_parser();
        let block = stmt.clone().repeated().collect().map(Stmt::Block);

        // POPRAWKA TUTAJ: Usunięto Box::new(index)
        let assignment = select! { Token::Ident(id) => id }
            .then(expr.clone().delimited_by(just(Token::LBracket), just(Token::RBracket)).or_not())
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .map(|((id, idx), val)| match idx {
                Some(index) => Stmt::ListAssignment(id, index, val), // index to już Expr, nie boxujemy go
                None => Stmt::Assignment(id, val),
            });

        let ret = just(Token::Return)
            .ignore_then(expr.clone().delimited_by(just(Token::LParen), just(Token::RParen)))
            .map(Stmt::Return);

        let if_stmt = just(Token::If).ignore_then(expr.clone()).then_ignore(just(Token::Then))
            .then(stmt.clone().map(Box::new))
            .then(just(Token::Else).ignore_then(stmt.clone().map(Box::new)).or_not())
            .map(|((cond, then_body), else_body)| Stmt::If { cond, then_body, else_body });

        let for_a = just(Token::For).ignore_then(select!{Token::Ident(i)=>i}).then_ignore(just(Token::From))
            .then(expr.clone()).then_ignore(just(Token::To)).then(expr.clone())
            .then_ignore(just(Token::Do)).then(block.clone()).then_ignore(just(Token::Od))
            .map(|(((var, start), end), body)| Stmt::For { var, start, end, down: false, body: Box::new(body) });

        let for_b = just(Token::For).ignore_then(select!{Token::Ident(i)=>i}).then_ignore(just(Token::Eq))
            .then(expr.clone()).then_ignore(just(Token::Downto)).then(expr.clone())
            .then_ignore(just(Token::Do)).then(block.clone()).then_ignore(just(Token::End))
            .map(|(((var, start), end), body)| Stmt::For { var, start, end, down: true, body: Box::new(body) });

        choice((
            assignment, ret, for_a, for_b, if_stmt, expr.map(Stmt::Expr),
        )).then_ignore(just(Token::Semi).or_not())
    })
}