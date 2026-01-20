use std::collections::{HashMap, HashSet};
use std::fs;
use crate::ast::{Expr, Op, Stmt};
use colored::*;
use rust_decimal::prelude::*;
use rust_decimal_macros::dec;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct DualValue {
    pub precise: Decimal,
    pub binary: f64,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(DualValue),
    Bool(bool),
    List(HashMap<i64, DualValue>),
    // Funkcja: (Nazwy argumentów, Ciało, Domknięcie środowiska)
    Function(Vec<String>, Box<Stmt>, HashMap<String, Value>), 
}

type Env = HashMap<String, Value>;

pub struct Interpreter {
    env: Env,
    // Zbiór do śledzenia plików wczytanych przez include_once
    included_files: HashSet<String>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self { 
            env: HashMap::new(),
            included_files: HashSet::new(),
        }
    }

    pub fn interpret(&mut self, program: Vec<Stmt>) -> Result<Value, String> {
        let mut last_result = Value::Number(DualValue { precise: dec!(0), binary: 0.0 });
        // Resetujemy wczytane pliki przy nowym uruchomieniu (opcjonalne)
        // self.included_files.clear(); 

        for stmt in program {
            match &stmt {
                Stmt::Expr(expr) => {
                    last_result = eval_expr(expr, &mut self.env, &mut self.included_files)?;
                }
                _ => {
                    if let Some(val) = eval_stmt(&stmt, &mut self.env, &mut self.included_files)? {
                        return Ok(val);
                    }
                }
            }
        }
        Ok(last_result)
    }
}

pub fn format_value(val: Value) -> String {
    match val {
        Value::Bool(b) => if b { 
            "True".yellow().to_string() 
        } else { 
            "False".yellow().to_string() 
        },
        Value::Number(dv) => format_dual(dv),
        Value::List(l) => format!("{:?}", l),
        Value::Function(args, _, _) => format!("Function({})", args.join(", ")).blue().to_string(),
    }
}

pub fn format_dual(val: DualValue) -> String {
    if val.binary.is_infinite() {
        let text = if val.binary.is_sign_positive() { "inf" } else { "-inf" };
        return text.red().to_string();
    }
    if val.binary.is_nan() { return "NaN".red().to_string(); }

    let diff = (val.binary - val.precise.to_f64().unwrap_or(0.0)).abs();
    if diff < 1e-12 {
        let is_integer = (val.binary.round() - val.binary).abs() < f64::EPSILON;
        if is_integer { return format!("{}", val.binary); }
    }

    let p_str = val.precise.to_string();
    let b_str = format!("{:.20}", val.binary).trim_end_matches('0').trim_end_matches('.').to_string();

    if diff < 1e-14 && p_str == b_str { return p_str; }

    let mut result = String::new();
    let p_chars: Vec<char> = p_str.chars().collect();
    let b_chars: Vec<char> = b_str.chars().collect();
    let mut divergence_found = false;

    for (i, &c_bin) in b_chars.iter().enumerate() {
        if divergence_found { result.push_str(&c_bin.to_string().red().to_string()); } 
        else if i < p_chars.len() && p_chars[i] == c_bin { result.push(c_bin); } 
        else {
            divergence_found = true;
            result.push_str(&c_bin.to_string().red().to_string());
        }
    }
    result
}

// Główna funkcja wykonująca instrukcje
// Wymaga przekazywania included_files, aby include działało rekurencyjnie
fn eval_stmt(stmt: &Stmt, env: &mut Env, included: &mut HashSet<String>) -> Result<Option<Value>, String> {
    match stmt {
        Stmt::Expr(expr) => { eval_expr(expr, env, included)?; Ok(None) }
        
        Stmt::Assignment(name, expr) => {
            let val = eval_expr(expr, env, included)?;
            env.insert(name.clone(), val);
            Ok(None)
        }

        Stmt::ListAssignment(name, idx_expr, val_expr) => {
            let idx = get_index(eval_expr(idx_expr, env, included)?)?;
            let val = get_number_dv(eval_expr(val_expr, env, included)?)?;
            let entry = env.entry(name.clone()).or_insert(Value::List(HashMap::new()));
            if let Value::List(map) = entry {
                if val.binary == 0.0 { map.remove(&idx); } 
                else { map.insert(idx, val); }
            } else { return Err(format!("'{}' nie jest listą", name)); }
            Ok(None)
        }

        Stmt::Return(expr) => Ok(Some(eval_expr(expr, env, included)?)),

        Stmt::Block(stmts) => {
            for s in stmts {
                if let Some(val) = eval_stmt(s, env, included)? { return Ok(Some(val)); }
            }
            Ok(None)
        }

        Stmt::If { cond, then_body, else_body } => {
            let c_val = get_bool(eval_expr(cond, env, included)?)?;
            if c_val { eval_stmt(then_body, env, included) }
            else if let Some(else_b) = else_body { eval_stmt(else_b, env, included) }
            else { Ok(None) }
        }

        // --- WHILE ---
        Stmt::While { cond, body } => {
            while get_bool(eval_expr(cond, env, included)?)? {
                if let Some(ret) = eval_stmt(body, env, included)? { return Ok(Some(ret)); }
            }
            Ok(None)
        }

        Stmt::For { var, start, end, down, body } => {
            let s_val = get_number_dv(eval_expr(start, env, included)?)?;
            let e_val = get_number_dv(eval_expr(end, env, included)?)?;
            
            env.insert(var.clone(), Value::Number(s_val));
            loop {
                let curr_val = match env.get(var) {
                    Some(Value::Number(dv)) => *dv,
                    _ => DualValue { precise: dec!(0), binary: 0.0 },
                };

                if *down { if curr_val.binary < e_val.binary { break; } } 
                else { if curr_val.binary > e_val.binary { break; } }

                if let Some(ret) = eval_stmt(body, env, included)? { return Ok(Some(ret)); }

                let one_dec = Decimal::new(1, 0);
                let next_p = if *down { curr_val.precise - one_dec } else { curr_val.precise + one_dec };
                let next_b = if *down { curr_val.binary - 1.0 } else { curr_val.binary + 1.0 };
                env.insert(var.clone(), Value::Number(DualValue { precise: next_p, binary: next_b }));
            }
            Ok(None)
        }

        // --- DEFINICJA FUNKCJI ---
        Stmt::Function(name, args, body) => {
            // Tworzymy funkcję z domknięciem (kopia obecnego środowiska)
            // Uwaga: Bez rekurencji "wprost" w domknięciu (prosty closure)
            let func_val = Value::Function(args.clone(), body.clone(), env.clone());
            env.insert(name.clone(), func_val);
            Ok(None)
        }

        // --- INCLUDE / INCLUDE_ONCE ---
        Stmt::Include(path) | Stmt::IncludeOnce(path) => {
            if let Stmt::IncludeOnce(_) = stmt {
                if included.contains(path) { return Ok(None); }
                included.insert(path.clone());
            }

            let content = fs::read_to_string(path)
                .map_err(|e| format!("Błąd include '{}': {}", path, e))?;

            // Używamy crate::lexer i crate::parser
            let tokens = crate::lexer::tokenize(&content)?;
            let ast = crate::parser::parse(tokens)?;

            // Wykonujemy kod z pliku w TYM SAMYM środowisku (env)
            for s in ast {
                if let Some(val) = eval_stmt(&s, env, included)? {
                    return Ok(Some(val));
                }
            }
            Ok(None)
        }
    }
}

fn eval_expr(expr: &Expr, env: &mut Env, included: &mut HashSet<String>) -> Result<Value, String> {
    match expr {
        Expr::Number(n) => {
            let dec_val = Decimal::from_f64(*n).unwrap_or(dec!(0));
            Ok(Value::Number(DualValue { precise: dec_val, binary: *n }))
        },
        Expr::Bool(b) => Ok(Value::Bool(*b)),
        Expr::Variable(name) => env.get(name).cloned().ok_or_else(|| format!("Błąd: Zmienna '{}' nieznana", name)),
        
        Expr::ListGet(name, idx_expr) => {
            let idx = get_index(eval_expr(idx_expr, env, included)?)?;
            match env.get(name) {
                Some(Value::List(map)) => {
                    let val = map.get(&idx).copied().unwrap_or(DualValue { precise: dec!(0), binary: 0.0 });
                    Ok(Value::Number(val))
                },
                Some(_) => Err(format!("'{}' nie jest listą", name)),
                None => Err(format!("Błąd: Lista '{}' nieznana", name)),
            }
        },

        // --- LAMBDA ---
        Expr::Lambda(args, body) => {
            // Lambda chwyta obecne środowisko (closure)
            Ok(Value::Function(args.clone(), body.clone(), env.clone()))
        },

        // --- WYWOŁANIE FUNKCJI ---
        Expr::Call(callee_expr, args_exprs) => {
            // 1. Ewaluacja funkcji (np. identyfikatora 'f' lub lambdy)
            let func_val = eval_expr(callee_expr, env, included)?;
            
            // 2. Rozpakowanie funkcji
            let (param_names, body, closure_env) = match func_val {
                Value::Function(p, b, e) => (p, b, e),
                _ => return Err("Próba wywołania czegoś, co nie jest funkcją".into()),
            };

            if param_names.len() != args_exprs.len() {
                return Err(format!("Oczekiwano {} argumentów, podano {}", param_names.len(), args_exprs.len()));
            }

            // 3. Ewaluacja argumentów w OBECNYM środowisku
            let mut arg_values = Vec::new();
            for arg_expr in args_exprs {
                arg_values.push(eval_expr(arg_expr, env, included)?);
            }

            // 4. Stworzenie nowego środowiska dla funkcji (Scope)
            // Bazujemy na closure_env (domknięciu), żeby lambda widziała zmienne z miejsca definicji
            let mut func_env = closure_env.clone();

            // 5. Wstrzyknięcie argumentów do środowiska funkcji
            for (name, val) in param_names.into_iter().zip(arg_values.into_iter()) {
                func_env.insert(name, val);
            }
            
            // Opcjonalnie: Rekurencja dla funkcji nazwanych
            // Jeśli wywołujemy zmienną po nazwie, możemy spróbować wstrzyknąć ją do środka,
            // ale w prostym modelu closure to wymagałoby większych zmian (Rc/RefCell).
            // Obecnie rekurencja zadziała, jeśli funkcja została zdefiniowana w globalnym scope.

            // 6. Wykonanie ciała funkcji
            if let Some(ret_val) = eval_stmt(&body, &mut func_env, included)? {
                Ok(ret_val)
            } else {
                // Jeśli funkcja nie ma Return, zwraca domyślnie 0.0
                Ok(Value::Number(DualValue { precise: dec!(0), binary: 0.0 }))
            }
        },

        Expr::Unary(op, e) => {
            let val = eval_expr(e, env, included)?;
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
            let l_val = eval_expr(lhs, env, included)?;
            let r_val = eval_expr(rhs, env, included)?;

            match op {
                // Arytmetyka
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
                
                // Logika
                Op::And | Op::Or => {
                    let l = get_bool(l_val)?;
                    let r = get_bool(r_val)?;
                    let res = match op {
                        Op::And => l && r,
                        Op::Or => l || r,
                        _ => unreachable!(),
                    };
                    Ok(Value::Bool(res))
                },

                // Porównania
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
                         let res = match op {
                            Op::Eq => l == r,
                            Op::Neq => l != r,
                            _ => return Err("Błąd: Operatory <, >, <=, >= nie działają dla typu Bool".into()),
                         };
                         Ok(Value::Bool(res))
                    } else {
                        Err("Błąd: Niezgodne typy w porównaniu".into())
                    }
                }
                
                _ => Err("Nieznany operator binarny".into())
            }
        }
    }
}

fn get_number_dv(v: Value) -> Result<DualValue, String> {
    match v { Value::Number(dv) => Ok(dv), _ => Err("Oczekiwano liczby".into()) }
}
fn get_bool(v: Value) -> Result<bool, String> {
    match v { Value::Bool(b) => Ok(b), _ => Err("Oczekiwano wartości logicznej (True/False)".into()) }
}
fn get_index(v: Value) -> Result<i64, String> {
    match v { Value::Number(dv) => Ok(dv.binary as i64), _ => Err("Indeks musi być liczbą".into()) }
}