use std::collections::HashMap;
use crate::ast::{Expr, Op, Stmt};
use colored::*;
use rust_decimal::prelude::*;
use rust_decimal_macros::dec;

#[derive(Debug, Clone, Copy)]
pub struct DualValue {
    pub precise: Decimal,
    pub binary: f64,
}

#[derive(Debug, Clone)]
pub enum Value {
    Number(DualValue),
    List(HashMap<i64, DualValue>),
}

type Env = HashMap<String, Value>;

pub struct Interpreter {
    env: Env,
}

impl Interpreter {
    // Konstruktor
    pub fn new() -> Self {
        Self {
            env: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, program: Vec<Stmt>) -> Result<DualValue, String> {
        let mut last_result = DualValue { precise: dec!(0), binary: 0.0 };

        for stmt in program {
            match &stmt {
                Stmt::Expr(expr) => {
                    let val = eval_expr(expr, &self.env)?;
                    if let Value::Number(dv) = val {
                        last_result = dv;
                    }
                }
                _ => {
                    if let Some(val) = eval_stmt(&stmt, &mut self.env)? {
                        return get_number_dv(val);
                    }
                }
            }
        }
        Ok(last_result)
    }
}

pub fn format_dual(val: DualValue) -> String {
    // 1. Obsługa Nieskończoności
    if val.binary.is_infinite() {
        let text = if val.binary.is_sign_positive() { "inf" } else { "-inf" };
        return text.red().to_string();
    }
    if val.binary.is_nan() {
        return "NaN".red().to_string();
    }

    let diff = (val.binary - val.precise.to_f64().unwrap_or(0.0)).abs();

    if diff < 1e-12 {
        let is_integer = (val.binary.round() - val.binary).abs() < f64::EPSILON;
        if is_integer {
            return format!("{}", val.binary);
        }
    }

    let p_str = val.precise.to_string();
    
    let b_str = format!("{:.20}", val.binary)
        .trim_end_matches('0')
        .trim_end_matches('.')
        .to_string();

    if diff < 1e-14 && p_str == b_str {
        return p_str;
    }

    let mut result = String::new();
    let p_chars: Vec<char> = p_str.chars().collect();
    let b_chars: Vec<char> = b_str.chars().collect();
    
    let mut divergence_found = false;

    for (i, &c_bin) in b_chars.iter().enumerate() {
        if divergence_found {
            result.push_str(&c_bin.to_string().red().to_string());
        } else {
            // Porównanie z idealną wartością
            if i < p_chars.len() && p_chars[i] == c_bin {
                result.push(c_bin);
            } else {
                divergence_found = true;
                result.push_str(&c_bin.to_string().red().to_string());
            }
        }
    }

    result
}

fn eval_stmt(stmt: &Stmt, env: &mut Env) -> Result<Option<Value>, String> {
    match stmt {
        Stmt::Expr(expr) => { eval_expr(expr, env)?; Ok(None) }
        
        Stmt::Assignment(name, expr) => {
            let val = eval_expr(expr, env)?;
            env.insert(name.clone(), val);
            Ok(None)
        }

        Stmt::ListAssignment(name, idx_expr, val_expr) => {
            let idx = get_index(eval_expr(idx_expr, env)?)?;
            let val = get_number_dv(eval_expr(val_expr, env)?)?;

            let entry = env.entry(name.clone()).or_insert(Value::List(HashMap::new()));
            
            if let Value::List(map) = entry {
                if val.binary == 0.0 { map.remove(&idx); } 
                else { map.insert(idx, val); }
            } else {
                return Err(format!("Zmienna '{}' nie jest listą", name));
            }
            Ok(None)
        }

        Stmt::Return(expr) => Ok(Some(eval_expr(expr, env)?)),

        Stmt::Block(stmts) => {
            for s in stmts {
                if let Some(val) = eval_stmt(s, env)? { return Ok(Some(val)); }
            }
            Ok(None)
        }

        Stmt::If { cond, then_body, else_body } => {
            let c_val = get_number_dv(eval_expr(cond, env)?)?;
            if c_val.binary != 0.0 { eval_stmt(then_body, env) }
            else if let Some(else_b) = else_body { eval_stmt(else_b, env) }
            else { Ok(None) }
        }

        Stmt::For { var, start, end, down, body } => {
            let s_val = get_number_dv(eval_expr(start, env)?)?;
            let e_val = get_number_dv(eval_expr(end, env)?)?;
            
            env.insert(var.clone(), Value::Number(s_val));
            
            loop {
                let curr_val = match env.get(var) {
                    Some(Value::Number(dv)) => *dv,
                    _ => DualValue { precise: dec!(0), binary: 0.0 },
                };

                if *down { if curr_val.binary < e_val.binary { break; } } 
                else { if curr_val.binary > e_val.binary { break; } }

                if let Some(ret) = eval_stmt(body, env)? { return Ok(Some(ret)); }

                let one_dec = Decimal::new(1, 0);
                let next_precise = if *down { curr_val.precise - one_dec } else { curr_val.precise + one_dec };
                let next_binary = if *down { curr_val.binary - 1.0 } else { curr_val.binary + 1.0 };
                
                env.insert(var.clone(), Value::Number(DualValue { precise: next_precise, binary: next_binary }));
            }
            Ok(None)
        }
    }
}

fn eval_expr(expr: &Expr, env: &Env) -> Result<Value, String> {
    match expr {
        Expr::Number(n) => {
            let dec_val = Decimal::from_f64(*n).unwrap_or(dec!(0));
            Ok(Value::Number(DualValue { precise: dec_val, binary: *n }))
        },
        Expr::Variable(name) => {
            env.get(name).cloned().ok_or_else(|| format!("Błąd: Zmienna '{}' nie zdefiniowana", name))
        },
        Expr::ListGet(name, idx_expr) => {
            let idx = get_index(eval_expr(idx_expr, env)?)?;
            match env.get(name) {
                Some(Value::List(map)) => {
                    let val = map.get(&idx).copied().unwrap_or(DualValue { precise: dec!(0), binary: 0.0 });
                    Ok(Value::Number(val))
                },
                Some(_) => Err(format!("'{}' nie jest listą", name)),
                None => Err(format!("Błąd: Lista '{}' nie zdefiniowana", name)),
            }
        },
        Expr::Unary(e) => {
            let val = get_number_dv(eval_expr(e, env)?)?;
            Ok(Value::Number(DualValue { precise: -val.precise, binary: -val.binary }))
        },
        Expr::Binary(lhs, op, rhs) => {
            let l = get_number_dv(eval_expr(lhs, env)?)?;
            let r = get_number_dv(eval_expr(rhs, env)?)?;
            let (res_p, res_b) = match op {
                Op::Add => (l.precise + r.precise, l.binary + r.binary),
                Op::Sub => (l.precise - r.precise, l.binary - r.binary),
                Op::Mul => (l.precise * r.precise, l.binary * r.binary),
                Op::Div => {
                    let bin_res = l.binary / r.binary;
                    let dec_res = if r.precise.is_zero() { dec!(0) } else { l.precise / r.precise };
                    (dec_res, bin_res)
                },
                Op::Pow => {
                    let res_bin = l.binary.powf(r.binary);
                    let res_dec = Decimal::from_f64(res_bin).unwrap_or(dec!(0));
                    (res_dec, res_bin)
                }
            };
            Ok(Value::Number(DualValue { precise: res_p, binary: res_b }))
        }
    }
}

fn get_number_dv(v: Value) -> Result<DualValue, String> {
    match v { Value::Number(dv) => Ok(dv), _ => Err("Oczekiwano liczby".into()) }
}

fn get_index(v: Value) -> Result<i64, String> {
    match v { Value::Number(dv) => Ok(dv.binary as i64), _ => Err("Indeks musi być liczbą".into()) }
}