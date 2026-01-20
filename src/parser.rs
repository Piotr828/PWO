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
        let ident = select! { Token::Ident(id) => id };

        let val = select! {
            Token::Number(n) => Expr::Number(n),
            Token::True => Expr::Bool(true),
            Token::False => Expr::Bool(false),
            Token::Ident(id) => Expr::Variable(id),
        };
        
        let lambda = just(Token::Pipe)
            .ignore_then(ident.clone().separated_by(just(Token::Comma)).collect())
            .then_ignore(just(Token::Pipe))
            .then_ignore(just(Token::Arrow))
            .then(expr.clone())
            .map(|(args, body_expr)| Expr::Lambda(args, Box::new(Stmt::Return(body_expr))));

        let atom_base = choice((
            lambda,
            val,
            expr.clone().delimited_by(just(Token::LParen), just(Token::RParen)),
            ident.clone()
                .then(expr.clone().delimited_by(just(Token::LBracket), just(Token::RBracket)))
                .map(|(id, idx)| Expr::ListGet(id, Box::new(idx))),
        ));

        let call_args = expr.clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>() 
            .delimited_by(just(Token::LParen), just(Token::RParen));

        let atom = atom_base.foldl(
            call_args.repeated(), 
            |callee, args| Expr::Call(Box::new(callee), args)
        );

        let unary = choice((
            just(Token::Minus).to(Op::Sub),
            just(Token::Not).to(Op::Not),
        ))
        .then(atom.clone())
        .map(|(op, val)| Expr::Unary(op, Box::new(val)))
        .or(atom);

        let power = unary.clone().then(just(Token::Caret).to(Op::Pow).then(unary).repeated().collect::<Vec<_>>())
            .map(|(first, rest)| rest.into_iter().fold(first, |l, (op, r)| Expr::Binary(Box::new(l), op, Box::new(r))));

        let op_mul = choice((just(Token::Star).to(Op::Mul), just(Token::Slash).to(Op::Div), just(Token::Mod).to(Op::Mod)));
        let product = power.clone().then(op_mul.then(power).repeated().collect::<Vec<_>>())
            .map(|(first, rest)| rest.into_iter().fold(first, |l, (op, r)| Expr::Binary(Box::new(l), op, Box::new(r))));

        let op_sum = choice((just(Token::Plus).to(Op::Add), just(Token::Minus).to(Op::Sub)));
        let sum = product.clone().then(op_sum.then(product).repeated().collect::<Vec<_>>())
            .map(|(first, rest)| rest.into_iter().fold(first, |l, (op, r)| Expr::Binary(Box::new(l), op, Box::new(r))));

        let op_cmp = choice((
            just(Token::Eq).to(Op::Eq), just(Token::Neq).to(Op::Neq),
            just(Token::Lte).to(Op::Lte), just(Token::Gte).to(Op::Gte),
            just(Token::Lt).to(Op::Lt), just(Token::Gt).to(Op::Gt),
        ));
        let comparison = sum.clone().then(op_cmp.then(sum).repeated().collect::<Vec<_>>())
            .map(|(first, rest)| rest.into_iter().fold(first, |l, (op, r)| Expr::Binary(Box::new(l), op, Box::new(r))));

        let logical_and = comparison.clone().then(just(Token::And).to(Op::And).then(comparison).repeated().collect::<Vec<_>>())
            .map(|(first, rest)| rest.into_iter().fold(first, |l, (op, r)| Expr::Binary(Box::new(l), op, Box::new(r))));

        let logical_or = logical_and.clone().then(just(Token::Or).to(Op::Or).then(logical_and).repeated().collect::<Vec<_>>())
            .map(|(first, rest)| rest.into_iter().fold(first, |l, (op, r)| Expr::Binary(Box::new(l), op, Box::new(r))));

        logical_or
    })
}

fn stmt_parser<'src>() -> impl Parser<'src, ParserInput<'src>, Stmt, ParserExtra<'src>> {
    recursive(|stmt| {
        let expr = expr_parser();
        let block = stmt.clone().repeated().collect().map(Stmt::Block);
        let ident = select! { Token::Ident(id) => id };

        let include = just(Token::Include).ignore_then(ident.clone()).map(Stmt::Include);
        let include_once = just(Token::IncludeOnce).ignore_then(ident.clone()).map(Stmt::IncludeOnce);

        let ret = just(Token::Return)
            .ignore_then(expr.clone().delimited_by(just(Token::LParen), just(Token::RParen)))
            .map(Stmt::Return);

        // --- POPRAWKA: Przypisania używają backtracking'u ---
        let list_assignment = ident.clone()
            .then(expr.clone().delimited_by(just(Token::LBracket), just(Token::RBracket)))
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .map(|((id, idx), val)| Stmt::ListAssignment(id, idx, val));

        let assignment = ident.clone()
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .map(|(id, val)| Stmt::Assignment(id, val));

        let if_end = just(Token::Fi).or(just(Token::End));
        let if_stmt = just(Token::If)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::Then))
            .then(block.clone())
            .then(just(Token::Else).ignore_then(block.clone()).or_not())
            .then_ignore(if_end)
            .map(|((cond, then_body), else_body)| Stmt::If { cond, then_body: Box::new(then_body), else_body: else_body.map(Box::new) });

        let loop_end = just(Token::Od).or(just(Token::End));
        
        let while_stmt = just(Token::While)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::Do))
            .then(block.clone())
            .then_ignore(loop_end.clone())
            .map(|(cond, body)| Stmt::While { cond, body: Box::new(body) });

        let for_variant_a = just(Token::For)
            .ignore_then(ident.clone())
            .then_ignore(just(Token::From))
            .then(expr.clone())
            .then_ignore(just(Token::To))
            .then(expr.clone())
            .then_ignore(just(Token::Do))
            .then(block.clone())
            .then_ignore(loop_end.clone())
            .map(|(((var, start), end), body)| Stmt::For { var, start, end, down: false, body: Box::new(body) });

        let for_variant_b = just(Token::For)
            .ignore_then(ident.clone())
            .then_ignore(just(Token::Eq))
            .then(expr.clone())
            .then_ignore(just(Token::Downto))
            .then(expr.clone())
            .then_ignore(just(Token::Do))
            .then(block.clone())
            .then_ignore(loop_end)
            .map(|(((var, start), end), body)| Stmt::For { var, start, end, down: true, body: Box::new(body) });

        let function_def = just(Token::Fn)
            .ignore_then(ident.clone())
            .then(ident.clone().separated_by(just(Token::Comma)).collect().delimited_by(just(Token::LParen), just(Token::RParen)))
            .then(block.clone())
            .then_ignore(just(Token::End))
            .map(|((name, args), body)| Stmt::Function(name, args, Box::new(body)));

        // Kolejność w choice jest kluczowa. Chumsky spróbuje dopasować po kolei.
        choice((
            include, include_once,
            function_def,
            // Próba przypisania do listy. Jeśli nie ma ":=", zawiedzie i przejdzie dalej.
            list_assignment, 
            assignment, 
            ret, 
            while_stmt, 
            for_variant_a, for_variant_b, 
            if_stmt, 
            // Na samym końcu sprawdzamy zwykłe wyrażenie (np. tablica[3])
            expr.map(Stmt::Expr)
        ))
        .then_ignore(just(Token::Semi).or_not())
    })
}