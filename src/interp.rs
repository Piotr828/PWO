use crate::ast::{Expr, Op};

pub fn interpret(ast: Expr) -> Result<f64, String> {
    match ast {
        Expr::Number(n) => Ok(n),
        
        Expr::Unary(expr) => {
            let val = interpret(*expr)?;
            Ok(-val)
        }
        
        Expr::Binary(lhs, op, rhs) => {
            let l = interpret(*lhs)?;
            let r = interpret(*rhs)?;
            
            match op {
                Op::Add => Ok(l + r),
                Op::Sub => Ok(l - r),
                Op::Mul => Ok(l * r),
                Op::Div => {
                    if r == 0.0 {
                        Err("Błąd: Dzielenie przez zero".into())
                    } else {
                        Ok(l / r)
                    }
                }
            }
        }
    }
}