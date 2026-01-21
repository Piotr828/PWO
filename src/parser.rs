use crate::ast::{Expr, Op, Stmt, CastSpec, CastType, CastMode};
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

        // --- NOWOŚĆ: Parsowanie literałów list [a, b, c] ---
        let list_literal = expr.clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .map(Expr::ListLiteral);

        let atom_base = choice((
            lambda,
            list_literal, // Dodajemy listę do bazy atomów
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

        // --- NOWOŚĆ: Parsowanie rzutowania (i32) atom ---
        // Token::Cast jest generowany przez lexer, gdy napotka np. (i32)
        let cast = select! {
            Token::Cast(c, s, d) => (c, s, d)
        }
        .then(atom.clone())
        .map(|((type_char, size, is_dec), val)| {
            let cast_type = match type_char {
                'i' => CastType::Int,
                'u' => CastType::Uint,
                'f' => CastType::Float,
                _ => CastType::Int,
            };
            let mode = if is_dec { CastMode::Decimal } else { CastMode::Bits };
            Expr::Cast(CastSpec { cast_type, size, mode }, Box::new(val))
        });

        // Standardowe operatory unarne: -x, !x
        let standard_unary = choice((
            just(Token::Minus).to(Op::Sub),
            just(Token::Not).to(Op::Not),
        ))
        .then(atom.clone())
        .map(|(op, val)| Expr::Unary(op, Box::new(val)));

        // Unary łączy rzutowanie, operatory i atomy
        // Kolejność choice ma znaczenie (najpierw specyficzne, potem ogólne)
        let unary = choice((
            cast,
            standard_unary,
            atom
        ));

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

        logical_or.boxed() // .boxed() pomaga kompilatorowi przy rekurencji
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

        choice((
            include, include_once,
            function_def,
            list_assignment, 
            assignment, 
            ret, 
            while_stmt, 
            for_variant_a, for_variant_b, 
            if_stmt, 
            expr.map(Stmt::Expr)
        ))
        .then_ignore(just(Token::Semi).or_not())
        .boxed() // .boxed() na końcu parsera instrukcji
    })
}