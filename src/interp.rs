use std::rc::Rc;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fs;
use crate::ast::{Expr, Op, Stmt, CastSpec, CastType, CastMode};
use colored::*;
use bigdecimal::{BigDecimal, FromPrimitive, ToPrimitive, Zero, One};

// --- KONFIGURACJA ---
const MAX_PRECISION: i64 = 300;

// --- STRUKTURY DANYCH ---

#[derive(Debug)]
pub enum ExecResult {
    Next(Option<Value>),
    Return(Value),
}

#[derive(Debug, Clone, PartialEq)]
pub struct DualValue {
    pub precise: BigDecimal,
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
            Value::Number(n) => Value::Number(n.clone()),
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
    pub included_files: HashSet<String>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self { 
            global_env: Environment::new(None),
            included_files: HashSet::new(),
        }
    }

    pub fn interpret(&mut self, program: Vec<Stmt>) -> Result<Value, String> {
        let mut last_result = Value::Number(DualValue { precise: BigDecimal::zero(), binary: 0.0 });
        
        for stmt in program {
            match eval_stmt(&stmt, &self.global_env, &mut self.included_files)? {
                ExecResult::Next(val) => {
                    if let Some(v) = val { last_result = v; }
                },
                ExecResult::Return(val) => {
                    return Ok(val);
                }
            }
        }
        Ok(last_result)
    }
}

// --- FORMATOWANIE I KOLOROWANIE ---

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

    // 1. POPRAWKA DŁUGOŚCI: Usuwamy końcowe zera z wersji precyzyjnej do wyświetlania
    let raw_p_str = val.precise.to_string();
    let p_str = if raw_p_str.contains('.') {
        raw_p_str.trim_end_matches('0').trim_end_matches('.').to_string()
    } else {
        raw_p_str
    };
    
    // Generowanie stringa binarnego (bez zmian)
    let b_str = format!("{:.15}", val.binary) // 15 cyfr wystarczy dla f64
        .trim_end_matches('0')
        .trim_end_matches('.')
        .to_string();

    // Sprawdzenie części całkowitej
    let get_int_part = |s: &str| s.split('.').next().unwrap_or("0").to_string();
    if get_int_part(&p_str) != get_int_part(&b_str) {
        return b_str.red().to_string();
    }

    let mut result = String::new();
    let p_chars: Vec<char> = p_str.chars().collect();
    let b_chars: Vec<char> = b_str.chars().collect();
    let mut divergence = false;
    let mut significant_digits = 0;

    for (i, &c_prec) in p_chars.iter().enumerate() {
        // Logika cyfr znaczących
        if c_prec.is_digit(10) {
            if significant_digits > 0 || c_prec != '0' {
                significant_digits += 1;
            }
        }

        if divergence {
            result.push_str(&c_prec.to_string().red().to_string());
        } else {
            // 2. POPRAWKA KOLORU:
            // Jeśli string binarny się skończył, zakładamy, że wirtualnie są tam zera.
            // Dzięki temu 0.5 (bin) == 0.5000 (prec) nie będzie błędem.
            let c_bin = if i < b_chars.len() { 
                b_chars[i] 
            } else { 
                '0' 
            };

            // Porównujemy znak z precise z (rzeczywistym lub wirtualnym) znakiem binary
            let matches = c_bin == c_prec;

            if matches {
                // Dodatkowa estetyka: jeśli binarny się skończył, ale precyzyjny ma dalej zera,
                // wyświetlamy je na szaro (lub normalnie), żeby pokazać, że to "tylko precyzja".
                if i >= b_chars.len() {
                    result.push_str(&c_prec.to_string().truecolor(100, 100, 100).to_string());
                } else {
                    result.push(c_prec);
                }
            } else {
                // Jeśli przekroczyliśmy precyzję double (16 cyfr), to różnice są normalne
                // i nie powinny być na czerwono (chyba że to ewidentny błąd obliczeń).
                // W PWO++ przyjmujemy: po 16 cyfrach ufamy Precise.
                if significant_digits > 16 {
                    result.push_str(&c_prec.to_string().yellow().to_string()); // Ostrzegawczy żółty/pomarańczowy dla ogona
                } else {
                    divergence = true;
                    result.push_str(&c_prec.to_string().red().to_string());
                }
            }
        }
    }
    
    result
}

// --- LOGIKA WYKONAWCZA (STMT) ---

fn eval_stmt(stmt: &Stmt, env: &Env, inc: &mut HashSet<String>) -> Result<ExecResult, String> {
    match stmt {
        Stmt::Expr(expr) => { 
            let val = eval_expr(expr, env, inc)?; 
            Ok(ExecResult::Next(Some(val))) 
        }
        
        Stmt::Assignment(name, expr) => {
            let val = eval_expr(expr, env, inc)?;
            env.borrow_mut().vars.insert(name.clone(), val.deep_clone());
            Ok(ExecResult::Next(Some(val)))
        }

        Stmt::ListAssignment(name, idx_expr, val_expr) => {
            let list_val = env.borrow().get(name).ok_or(format!("Zmienna '{}' nie istnieje", name))?;
            
            if let Value::List(vec_rc) = list_val {
                let raw_idx = get_index(eval_expr(idx_expr, env, inc)?)?;
                let val = eval_expr(val_expr, env, inc)?;
                
                let mut vec = vec_rc.borrow_mut();
                let len = vec.len() as i64;
                
                let final_idx = if raw_idx < 0 { len + raw_idx } else { raw_idx };

                if final_idx < 0 {
                    return Err(format!("Ujemny indeks {} wykracza poza początek listy (długość: {})", raw_idx, len));
                }

                // Auto-rozszerzanie tylko dla indeksów dodatnich wykraczających w przód
                if raw_idx >= 0 && raw_idx >= len {
                    let new_len = (raw_idx + 1) as usize;
                    vec.resize(new_len, Value::Number(DualValue { precise: BigDecimal::zero(), binary: 0.0 }));
                } else if final_idx as usize >= vec.len() {
                    return Err(format!("Indeks {} poza zakresem", raw_idx));
                }
                
                vec[final_idx as usize] = val.deep_clone();
                Ok(ExecResult::Next(Some(val.deep_clone())))
            } else {
                Err(format!("'{}' nie jest listą", name))
            }
        }

        Stmt::Return(expr) => {
            let val = eval_expr(expr, env, inc)?;
            Ok(ExecResult::Return(val))
        }

        Stmt::Block(stmts) => {
            let mut last = None;
            for s in stmts {
                let res = eval_stmt(s, env, inc)?;
                match res {
                    ExecResult::Return(v) => return Ok(ExecResult::Return(v)),
                    ExecResult::Next(v) => last = v,
                }
            }
            Ok(ExecResult::Next(last))
        }

        Stmt::If { cond, then_body, else_body } => {
            if get_bool(eval_expr(cond, env, inc)?)? {
                eval_stmt(then_body, env, inc)
            } else if let Some(else_b) = else_body {
                eval_stmt(else_b, env, inc)
            } else {
                Ok(ExecResult::Next(None))
            }
        }

        Stmt::While { cond, body } => {
            let mut last = None;
            while get_bool(eval_expr(cond, env, inc)?)? {
                let res = eval_stmt(body, env, inc)?;
                match res {
                    ExecResult::Return(v) => return Ok(ExecResult::Return(v)),
                    ExecResult::Next(v) => if v.is_some() { last = v; }
                }
            }
            Ok(ExecResult::Next(last))
        }

        Stmt::For { var, start, end, down, body } => {
            let s_val = get_number_dv(eval_expr(start, env, inc)?)?;
            let e_val = get_number_dv(eval_expr(end, env, inc)?)?;
            
            env.borrow_mut().vars.insert(var.clone(), Value::Number(s_val.clone()));
            let mut last = None;
            
            loop {
                let curr_val = match env.borrow().get(var) {
                    Some(Value::Number(dv)) => dv,
                    _ => DualValue { precise: BigDecimal::zero(), binary: 0.0 },
                };

                if *down { if curr_val.binary < e_val.binary { break; } } 
                else { if curr_val.binary > e_val.binary { break; } }

                let res = eval_stmt(body, env, inc)?;
                match res {
                    ExecResult::Return(v) => return Ok(ExecResult::Return(v)),
                    ExecResult::Next(v) => if v.is_some() { last = v; }
                }

                let next_p = if *down { &curr_val.precise - BigDecimal::one() } else { &curr_val.precise + BigDecimal::one() };
                let next_b = if *down { curr_val.binary - 1.0 } else { curr_val.binary + 1.0 };
                
                env.borrow_mut().vars.insert(var.clone(), Value::Number(DualValue { precise: next_p, binary: next_b }));
            }
            Ok(ExecResult::Next(last))
        }

        Stmt::Function(name, args, body) => {
            let func_val = Value::Function(args.clone(), body.clone(), env.clone());
            env.borrow_mut().vars.insert(name.clone(), func_val);
            Ok(ExecResult::Next(None))
        }

        Stmt::Include(path) | Stmt::IncludeOnce(path) => {
            if let Stmt::IncludeOnce(_) = stmt {
                if inc.contains(path) { return Ok(ExecResult::Next(None)); }
                inc.insert(path.clone());
            }
            let content = fs::read_to_string(path).map_err(|e| format!("Błąd include '{}': {}", path, e))?;
            let tokens = crate::lexer::tokenize(&content)?;
            let ast = crate::parser::parse(tokens)?;
            
            let mut last = None;
            for s in ast {
                match eval_stmt(&s, env, inc)? {
                    ExecResult::Return(v) => return Ok(ExecResult::Return(v)),
                    ExecResult::Next(v) => if let Some(val) = v { last = Some(val); }
                }
            }
            Ok(ExecResult::Next(last))
        }
    }
}

// --- LOGIKA WYRAŻEŃ (EXPR) ---

fn eval_expr(expr: &Expr, env: &Env, inc: &mut HashSet<String>) -> Result<Value, String> {
    match expr {
        Expr::Number(n) => {
            let big_dec = BigDecimal::from_f64(*n).ok_or("Błąd konwersji f64->BigDecimal")?;
            Ok(Value::Number(DualValue { precise: big_dec, binary: *n }))
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
                let raw_idx = get_index(eval_expr(idx_expr, env, inc)?)?;
                let vec = vec_rc.borrow();
                let len = vec.len() as i64;
                
                let final_idx = if raw_idx < 0 { len + raw_idx } else { raw_idx };

                if final_idx < 0 || final_idx >= len {
                    return Err(format!("Indeks {} poza zakresem [0..{}]", raw_idx, len));
                }
                
                Ok(vec[final_idx as usize].clone())
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
                _ => return Err("Próba wywołania wartości, która nie jest funkcją".into()),
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

            let result = eval_stmt(&body, &func_env, inc)?;
            
            match result {
                ExecResult::Return(val) => Ok(val),
                ExecResult::Next(opt_val) => {
                    Ok(opt_val.unwrap_or(Value::Number(DualValue { 
                        precise: BigDecimal::zero(), 
                        binary: 0.0 
                    })))
                },
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
                        Op::Add => (l.precise + &r.precise, l.binary + r.binary),
                        Op::Sub => (l.precise - &r.precise, l.binary - r.binary),
                        Op::Mul => (l.precise * &r.precise, l.binary * r.binary),
                        Op::Div => {
                            if r.binary == 0.0 { return Err("Dzielenie przez zero".into()); }
                            let p = l.precise.with_scale(MAX_PRECISION) / r.precise;
                            (p, l.binary / r.binary)
                        },
                        Op::Mod => {
                            if r.binary == 0.0 { return Err("Modulo przez zero".into()); }
                            (l.precise % &r.precise, l.binary % r.binary)
                        },
                        Op::Pow => {
                            let rb = l.binary.powf(r.binary);
                            let pb = BigDecimal::from_f64(rb).unwrap_or(BigDecimal::zero());
                            (pb, rb)
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
                         let res = match op { Op::Eq => l == r, Op::Neq => l != r, _ => return Err("Błąd typów w porównaniu bool".into()) };
                         Ok(Value::Bool(res))
                    } else {
                        Err(format!("Niezgodne typy w porównaniu: {:?} vs {:?}", l_val, r_val))
                    }
                },
                Op::Not => Err("Operator '!' (Not) nie jest operatorem binarnym".into()),
            }
        }
    }
}

// --- LOGIKA RZUTOWANIA ---

fn apply_cast(val: DualValue, spec: &CastSpec) -> Result<DualValue, String> {
    let mut num_bin = val.binary;
    let mut num_prec = val.precise;

    match spec.mode {
        CastMode::Decimal => {
            match spec.cast_type {
                CastType::Float => {
                    let dp = spec.size as i64;
                    num_prec = num_prec.with_scale(dp);
                    num_bin = num_prec.to_f64().unwrap_or(num_bin);
                },
                CastType::Int | CastType::Uint => {
                    let mut range = BigDecimal::one();
                    let ten = BigDecimal::from(10);
                    for _ in 0..spec.size { range = &range * &ten; }
                    
                    num_prec = num_prec.with_scale(0); 
                    num_prec = num_prec % range.clone();

                    if spec.cast_type == CastType::Uint && num_prec < BigDecimal::zero() {
                        num_prec = num_prec + range;
                    }
                    
                    num_bin = num_prec.to_f64().unwrap_or(0.0);
                }
            }
        },
        CastMode::Bits => {
            match spec.cast_type {
                CastType::Float => {
                    match spec.size {
                        64 => { },
                        32 => { num_bin = (num_bin as f32) as f64; },
                        16 => {
                            let mut f_bits = (num_bin as f32).to_bits();
                            f_bits &= 0xFFFFE000; 
                            num_bin = f32::from_bits(f_bits) as f64;
                        },
                        8 => {
                            let mut f_bits = (num_bin as f32).to_bits();
                            f_bits &= 0xFFC00000; 
                            num_bin = f32::from_bits(f_bits) as f64;
                        },
                        _ => return Err(format!("Bity float: f64, f32, f16, f8. Otrzymano: f{}", spec.size)),
                    }
                    num_prec = BigDecimal::from_f64(num_bin).unwrap_or(num_prec);
                },
                CastType::Int | CastType::Uint => {
                    let range_f = 2f64.powf(spec.size as f64);
                    let range = BigDecimal::from_f64(range_f).unwrap_or(BigDecimal::from(1));
                    
                    num_prec = num_prec.with_scale(0) % &range;
                    
                    if spec.cast_type == CastType::Uint {
                        if num_prec < BigDecimal::zero() { num_prec += range; }
                    } else {
                        let half = range.clone() / BigDecimal::from(2);
                        if num_prec >= half { num_prec -= range; }
                        else if num_prec < -half { num_prec += range.clone(); }
                    }
                    num_bin = num_prec.to_f64().unwrap_or(0.0);
                }
            }
        }
    }

    Ok(DualValue { precise: num_prec, binary: num_bin })
}

fn get_number_dv(v: Value) -> Result<DualValue, String> { 
    match v { Value::Number(dv) => Ok(dv), _ => Err("Błąd typu: Oczekiwano liczby".into()) } 
}
fn get_bool(v: Value) -> Result<bool, String> { 
    match v { Value::Bool(b) => Ok(b), _ => Err("Błąd typu: Oczekiwano Boola".into()) } 
}
fn get_index(v: Value) -> Result<i64, String> { 
    match v { 
        Value::Number(dv) => {
            if !dv.binary.is_finite() {
                return Err("Indeks listy nie może być nieskończonością ani NaN".into());
            }
            Ok(dv.binary.round() as i64)
        }, 
        _ => Err("Błąd typu: Indeks musi być liczbą".into()) 
    } 
}