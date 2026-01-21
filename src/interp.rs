use std::rc::Rc;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fs;
use crate::ast::{Expr, Op, Stmt, CastSpec, CastType, CastMode};
use colored::*;
use rust_decimal::prelude::*;
use rust_decimal_macros::dec;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct DualValue {
    pub precise: Decimal,
    pub binary: f64,
}

#[derive(Debug, PartialEq)] 
pub struct Environment {
    pub vars: HashMap<String, Value>,
    pub parent: Option<Env>,
}

pub type Env = Rc<RefCell<Environment>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(DualValue),
    Bool(bool),
    List(Rc<RefCell<Vec<Value>>>),
    Function(Vec<String>, Box<Stmt>, Env), 
}

impl Environment {
    pub fn new(parent: Option<Env>) -> Env {
        Rc::new(RefCell::new(Environment {
            vars: HashMap::new(),
            parent,
        }))
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        if let Some(val) = self.vars.get(name) {
            Some(val.clone())
        } else if let Some(ref parent) = self.parent {
            parent.borrow().get(name)
        } else {
            None
        }
    }
}

impl Value {
    pub fn deep_clone(&self) -> Self {
        match self {
            Value::Number(n) => Value::Number(*n),
            Value::Bool(b) => Value::Bool(*b),
            Value::List(l) => {
                let cloned_vec = l.borrow().iter().map(|v| v.deep_clone()).collect();
                Value::List(Rc::new(RefCell::new(cloned_vec)))
            },
            Value::Function(args, body, env) => Value::Function(args.clone(), body.clone(), env.clone()),
        }
    }
}

pub struct Interpreter {
    pub global_env: Env,
    included_files: HashSet<String>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self { 
            global_env: Environment::new(None),
            included_files: HashSet::new(),
        }
    }

    pub fn interpret(&mut self, program: Vec<Stmt>) -> Result<Value, String> {
        let mut last_result = Value::Number(DualValue { precise: dec!(0), binary: 0.0 });
        for stmt in program {
            if let Some(val) = eval_stmt(&stmt, &self.global_env, &mut self.included_files)? {
                last_result = val;
            }
        }
        Ok(last_result)
    }
}


pub fn format_value(val: Value) -> String {
    match val {
        Value::Bool(b) => if b { "True".yellow().to_string() } else { "False".yellow().to_string() },
        Value::Number(dv) => format_dual(dv),
        Value::List(l) => {
            let items: Vec<_> = l.borrow().iter().map(|v| format_value(v.clone())).collect();
            format!("[{}]", items.join(", "))
        },
        Value::Function(args, _, _) => format!("fn({})", args.join(", ")).blue().to_string(),
    }
}

pub fn format_dual(val: DualValue) -> String {
    if val.binary.is_infinite() {
        let text = if val.binary.is_sign_positive() { "inf" } else { "-inf" };
        return text.red().to_string();
    }
    if val.binary.is_nan() { return "NaN".red().to_string(); }

    
    let p_str = val.precise.to_string();
    
    
    
    let b_str = format!("{:.20}", val.binary)
        .trim_end_matches('0')
        .trim_end_matches('.')
        .to_string();
    
    let get_int_part = |s: &str| s.split('.').next().unwrap_or("").to_string();
    
    
    if get_int_part(&p_str) != get_int_part(&b_str) {
        return b_str.red().to_string();
    }

    let mut result = String::new();
    let p_chars: Vec<char> = p_str.chars().collect();
    let b_chars: Vec<char> = b_str.chars().collect();
    let mut divergence = false;

    
    for (i, &c_bin) in b_chars.iter().enumerate() {
        if divergence {
            
            result.push_str(&c_bin.to_string().red().to_string());
        } else {
            
            if i < p_chars.len() {
                if c_bin == p_chars[i] {
                    result.push(c_bin); 
                } else {
                    divergence = true; 
                    result.push_str(&c_bin.to_string().red().to_string());
                }
            } else {
                
                divergence = true;
                result.push_str(&c_bin.to_string().red().to_string());
            }
        }
    }

    result
}



fn eval_stmt(stmt: &Stmt, env: &Env, inc: &mut HashSet<String>) -> Result<Option<Value>, String> {
    match stmt {
        Stmt::Expr(expr) => { 
            let val = eval_expr(expr, env, inc)?; 
            Ok(Some(val)) 
        }
        
        Stmt::Assignment(name, expr) => {
            let val = eval_expr(expr, env, inc)?;
            env.borrow_mut().vars.insert(name.clone(), val.deep_clone());
            Ok(Some(val))
        }

        
        Stmt::ListAssignment(name, idx_expr, val_expr) => {
            let list_val = env.borrow().get(name).ok_or(format!("Zmienna '{}' nie istnieje", name))?;
            
            if let Value::List(vec_rc) = list_val {
                let idx = get_index(eval_expr(idx_expr, env, inc)?)?;
                let val = eval_expr(val_expr, env, inc)?;
                
                let mut vec = vec_rc.borrow_mut();
                if idx < 0 {
                    return Err(format!("Indeks {} nie może być ujemny", idx));
                }
                
                
                if idx as usize >= vec.len() {
                    let new_len = idx as usize + 1;
                    vec.resize(new_len, Value::Number(DualValue { precise: dec!(0), binary: 0.0 }));
                }
                
                let val_clone = val.deep_clone();
                vec[idx as usize] = val_clone.clone();
                Ok(Some(val_clone))
            } else {
                return Err(format!("'{}' nie jest listą", name));
            }
        }

        Stmt::Return(expr) => Ok(Some(eval_expr(expr, env, inc)?)),

        Stmt::Block(stmts) => {
            let mut last = None;
            for s in stmts {
                let res = eval_stmt(s, env, inc)?;
                if let Stmt::Return(_) = s { return Ok(res); } 
                last = res;
            }
            Ok(last)
        }

        Stmt::If { cond, then_body, else_body } => {
            if get_bool(eval_expr(cond, env, inc)?)? {
                eval_stmt(then_body, env, inc)
            } else if let Some(else_b) = else_body {
                eval_stmt(else_b, env, inc)
            } else {
                Ok(None)
            }
        }

        Stmt::While { cond, body } => {
            let mut last = None;
            while get_bool(eval_expr(cond, env, inc)?)? {
                let res = eval_stmt(body, env, inc)?;
                if res.is_some() { last = res; }
            }
            Ok(last)
        }

        Stmt::For { var, start, end, down, body } => {
            let s_val = get_number_dv(eval_expr(start, env, inc)?)?;
            let e_val = get_number_dv(eval_expr(end, env, inc)?)?;
            
            env.borrow_mut().vars.insert(var.clone(), Value::Number(s_val));
            let mut last = None;
            
            loop {
                let curr_val = match env.borrow().get(var) {
                    Some(Value::Number(dv)) => dv,
                    _ => DualValue { precise: dec!(0), binary: 0.0 },
                };

                if *down { if curr_val.binary < e_val.binary { break; } } 
                else { if curr_val.binary > e_val.binary { break; } }

                let res = eval_stmt(body, env, inc)?;
                if res.is_some() { last = res; }

                let one_dec = Decimal::new(1, 0);
                let next_p = if *down { curr_val.precise - one_dec } else { curr_val.precise + one_dec };
                let next_b = if *down { curr_val.binary - 1.0 } else { curr_val.binary + 1.0 };
                
                env.borrow_mut().vars.insert(var.clone(), Value::Number(DualValue { precise: next_p, binary: next_b }));
            }
            Ok(last)
        }

        Stmt::Function(name, args, body) => {
            let func_val = Value::Function(args.clone(), body.clone(), env.clone());
            env.borrow_mut().vars.insert(name.clone(), func_val);
            Ok(None)
        }

        Stmt::Include(path) | Stmt::IncludeOnce(path) => {
            if let Stmt::IncludeOnce(_) = stmt {
                if inc.contains(path) { return Ok(None); }
                inc.insert(path.clone());
            }
            let content = fs::read_to_string(path).map_err(|e| format!("Błąd include '{}': {}", path, e))?;
            let tokens = crate::lexer::tokenize(&content)?;
            let ast = crate::parser::parse(tokens)?;
            
            let mut last = None;
            for s in ast {
                if let Some(val) = eval_stmt(&s, env, inc)? { last = Some(val); }
            }
            Ok(last)
        }
    }
}

fn eval_expr(expr: &Expr, env: &Env, inc: &mut HashSet<String>) -> Result<Value, String> {
    match expr {
        Expr::Number(n) => {
            let dec_val = Decimal::from_f64(*n).unwrap_or(dec!(0));
            Ok(Value::Number(DualValue { precise: dec_val, binary: *n }))
        },
        Expr::Bool(b) => Ok(Value::Bool(*b)),
        
        Expr::Variable(name) => {
            env.borrow().get(name).ok_or_else(|| format!("Błąd: Zmienna '{}' nieznana", name))
        },
        
        Expr::Cast(spec, inner_expr) => {
            let val = eval_expr(inner_expr, env, inc)?;
            let num = get_number_dv(val)?;
            
            let new_num = apply_cast(num, spec)?;
            Ok(Value::Number(new_num))
        },

        Expr::ListLiteral(elems) => {
            let mut values = Vec::new();
            for e in elems {
                values.push(eval_expr(e, env, inc)?);
            }
            Ok(Value::List(Rc::new(RefCell::new(values))))
        },

        Expr::ListGet(name, idx_expr) => {
            let list_val = env.borrow().get(name).ok_or(format!("Zmienna '{}' nie istnieje", name))?;
            if let Value::List(vec_rc) = list_val {
                let idx = get_index(eval_expr(idx_expr, env, inc)?)?;
                let vec = vec_rc.borrow();
                if idx < 0 || idx as usize >= vec.len() {
                    return Err(format!("Indeks {} poza zakresem", idx));
                }
                Ok(vec[idx as usize].clone())
            } else {
                Err(format!("'{}' nie jest listą", name))
            }
        },

        Expr::Lambda(args, body) => {
            Ok(Value::Function(args.clone(), body.clone(), env.clone()))
        },

        Expr::Call(callee_expr, args_exprs) => {
            let func_val = eval_expr(callee_expr, env, inc)?;
            
            let (param_names, body, closure_env) = match func_val {
                Value::Function(p, b, e) => (p, b, e),
                _ => return Err("Próba wywołania czegoś, co nie jest funkcją".into()),
            };

            if param_names.len() != args_exprs.len() {
                return Err(format!("Oczekiwano {} argumentów, podano {}", param_names.len(), args_exprs.len()));
            }

            let mut arg_values = Vec::new();
            for arg_expr in args_exprs {
                arg_values.push(eval_expr(arg_expr, env, inc)?);
            }

            let func_env = Environment::new(Some(closure_env));

            for (name, val) in param_names.into_iter().zip(arg_values.into_iter()) {
                func_env.borrow_mut().vars.insert(name, val);
            }

            if let Some(ret_val) = eval_stmt(&body, &func_env, inc)? {
                Ok(ret_val)
            } else {
                Ok(Value::Number(DualValue { precise: dec!(0), binary: 0.0 }))
            }
        },

        Expr::Unary(op, e) => {
            let val = eval_expr(e, env, inc)?;
            match op {
                Op::Sub => {
                    let v = get_number_dv(val)?;
                    Ok(Value::Number(DualValue { precise: -v.precise, binary: -v.binary }))
                },
                Op::Not => {
                    let v = get_bool(val)?;
                    Ok(Value::Bool(!v))
                },
                _ => Err("Błędny operator unarny".into())
            }
        },

        Expr::Binary(lhs, op, rhs) => {
            let l_val = eval_expr(lhs, env, inc)?;
            let r_val = eval_expr(rhs, env, inc)?;

            match op {
                Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Mod | Op::Pow => {
                    let l = get_number_dv(l_val)?;
                    let r = get_number_dv(r_val)?;
                    let (res_p, res_b) = match op {
                        Op::Add => (l.precise + r.precise, l.binary + r.binary),
                        Op::Sub => (l.precise - r.precise, l.binary - r.binary),
                        Op::Mul => (l.precise * r.precise, l.binary * r.binary),
                        Op::Div => {
                            if r.binary == 0.0 { return Err("Dzielenie przez zero".into()); }
                            let dec_res = if r.precise.is_zero() { dec!(0) } else { l.precise / r.precise };
                            (dec_res, l.binary / r.binary)
                        },
                        Op::Mod => {
                            if r.binary == 0.0 { return Err("Modulo przez zero".into()); }
                            (l.precise % r.precise, l.binary % r.binary)
                        },
                        Op::Pow => {
                            let rb = l.binary.powf(r.binary);
                            (Decimal::from_f64(rb).unwrap_or(dec!(0)), rb)
                        },
                        _ => unreachable!(),
                    };
                    Ok(Value::Number(DualValue { precise: res_p, binary: res_b }))
                },
                Op::And | Op::Or => {
                    let l = get_bool(l_val)?;
                    let r = get_bool(r_val)?;
                    let res = match op { Op::And => l && r, Op::Or => l || r, _ => unreachable!() };
                    Ok(Value::Bool(res))
                },
                Op::Eq | Op::Neq | Op::Lt | Op::Gt | Op::Lte | Op::Gte => {
                    if let (Value::Number(l), Value::Number(r)) = (&l_val, &r_val) {
                        let res = match op {
                            Op::Eq => (l.binary - r.binary).abs() < f64::EPSILON,
                            Op::Neq => (l.binary - r.binary).abs() >= f64::EPSILON,
                            Op::Lt => l.binary < r.binary,
                            Op::Gt => l.binary > r.binary,
                            Op::Lte => l.binary <= r.binary,
                            Op::Gte => l.binary >= r.binary,
                            _ => unreachable!(),
                        };
                        Ok(Value::Bool(res))
                    } else if let (Value::Bool(l), Value::Bool(r)) = (&l_val, &r_val) {
                         let res = match op { Op::Eq => l == r, Op::Neq => l != r, _ => return Err("Błąd typów w porównaniu".into()) };
                         Ok(Value::Bool(res))
                    } else {
                        Err("Niezgodne typy w porównaniu".into())
                    }
                },
                Op::Not => Err("Błąd składni: Operator '!' (Not) nie jest operatorem binarnym".into()),
            }
        }
    }
}



fn apply_cast(val: DualValue, spec: &CastSpec) -> Result<DualValue, String> {
    let mut num = val.binary;

    match spec.mode {
        CastMode::Decimal => {
            let factor = 10f64.powi(spec.size as i32);
            num = (num * factor).round() / factor;
        },
        CastMode::Bits => {
            match spec.cast_type {
                CastType::Int => {
                    let bits = spec.size;
                    let range = 2f64.powf(bits as f64);
                    let half_range = range / 2.0;
                    
                    num = num % range;
                    if num >= half_range {
                        num -= range;
                    } else if num < -half_range {
                        num += range;
                    }
                    num = num.trunc();
                },
                CastType::Uint => {
                    let range = 2f64.powf(spec.size as f64);
                    num = num % range;
                    if num < 0.0 { num += range; }
                    num = num.trunc();
                },
                CastType::Float => {
                    match spec.size {
                        16 => {
                            let mut f_bits = (num as f32).to_bits();
                            f_bits &= 0xFFFFE000; 
                            num = f32::from_bits(f_bits) as f64;
                        },
                        32 => {
                            num = (num as f32) as f64;
                        },
                        64 => { },
                        _ => return Err(format!("Dostępne floaty: f16, f32, f64")),
                    }
                }
            }
        }
    }

    
    
    Ok(DualValue { precise: val.precise, binary: num })
}

fn get_number_dv(v: Value) -> Result<DualValue, String> { match v { Value::Number(dv) => Ok(dv), _ => Err("Oczekiwano liczby".into()) } }
fn get_bool(v: Value) -> Result<bool, String> { match v { Value::Bool(b) => Ok(b), _ => Err("Oczekiwano Boola".into()) } }
fn get_index(v: Value) -> Result<i64, String> { match v { Value::Number(dv) => Ok(dv.binary as i64), _ => Err("Indeks musi być liczbą".into()) } }