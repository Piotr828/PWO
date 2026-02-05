use std::rc::Rc;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fs;
use crate::ast::{Expr, Op, Stmt, CastSpec, CastType, CastMode};
use colored::*;
use bigdecimal::{BigDecimal, FromPrimitive, ToPrimitive, Zero, One};
use chrono::{Local, Datelike};

// --- KONFIGURACJA ---

/// Maksymalna precyzja używana przy obliczeniach na BigDecimal.
const MAX_PRECISION: i64 = 300;

// --- STRUKTURY DANYCH ---

/// Wynik wykonania pojedynczej instrukcji.
/// Służy do sterowania przepływem (np. obsługa `return`).
#[derive(Debug)]
pub enum ExecResult {
    /// Przejdź do kolejnej instrukcji (opcjonalnie z wartością).
    Next(Option<Value>),
    /// Przerwij wykonywanie funkcji i zwróć wartość.
    Return(Value),
}

/// Podwójna reprezentacja liczby.
/// Przechowuje jednocześnie wartość precyzyjną (BigDecimal) oraz przybliżoną (f64),
/// co pozwala na wizualizację błędów standardu IEEE-754.
#[derive(Debug, Clone, PartialEq)]
pub struct DualValue {
    /// Wartość o dowolnej precyzji (matematycznie poprawna).
    pub precise: BigDecimal,
    /// Wartość binarna (standardowy float 64-bit).
    pub binary: f64,
}

/// Środowisko wykonawcze przechowywujące zmienne.
/// Obsługuje zagnieżdżanie zakresów (scopes) poprzez wskaźnik `parent`.
#[derive(Debug, PartialEq)]
pub struct Environment {
    pub vars: HashMap<String, Value>,
    pub parent: Option<Env>,
}

/// Typ pomocniczy dla współdzielonego, mutowalnego środowiska.
pub type Env = Rc<RefCell<Environment>>;

/// Typy wartości obsługiwane przez interpreter.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// Liczba (z podwójną precyzją).
    Number(DualValue),
    /// Wartość logiczna.
    Bool(bool),
    /// Lista (współdzielona referencja do wektora wartości).
    List(Rc<RefCell<Vec<Value>>>),
    /// Funkcja zdefiniowana przez użytkownika (argumenty, ciało, domknięcie).
    Function(Vec<String>, Box<Stmt>, Env),
}

impl Environment {
    /// Tworzy nowe środowisko, opcjonalnie dziedziczące po rodzicu.
    pub fn new(parent: Option<Env>) -> Env {
        Rc::new(RefCell::new(Environment {
            vars: HashMap::new(),
            parent,
        }))
    }

    /// Pobiera wartość zmiennej, przeszukując rekurencyjnie rodziców.
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
    /// Tworzy głęboką kopię wartości.
    /// Jest to kluczowe dla list, aby uniknąć niezamierzonego współdzielenia referencji przy przypisaniu.
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

/// Główna struktura interpretera.
/// Przechowuje globalne środowisko i stan dołączonych plików.
pub struct Interpreter {
    pub global_env: Env,
    pub included_files: HashSet<String>,
}

impl Interpreter {
    /// Inicjalizuje interpreter z domyślnym środowiskiem globalnym.
    /// Definiuje stałe systemowe: YEAR, e, pi.
    pub fn new() -> Self {

        let env = Environment::new(None);

        {
            let vars = &mut env.borrow_mut().vars;

            // --- YEAR (Aktualny rok) ---
            let now = Local::now();
            let current_year = now.year();
            
            vars.insert("YEAR".to_string(), Value::Number(DualValue {
                precise: BigDecimal::from(current_year),
                binary: current_year as f64,
            }));

            // --- e (Liczba Eulera) ---
            let e_val = std::f64::consts::E;
            vars.insert("e".to_string(), Value::Number(DualValue {
                precise: BigDecimal::from_f64(e_val).unwrap(), // unwrap jest tu bezpieczny dla stałej
                binary: e_val,
            }));

            let pi_val = std::f64::consts::PI;
            vars.insert("pi".to_string(), Value::Number(DualValue {
                precise: BigDecimal::from_f64(pi_val).unwrap(),
                binary: pi_val,
            }));
        }

        Self { 
            global_env: env,
            included_files: HashSet::new(),
        }
    }

    /// Wykonuje przekazany program (listę instrukcji).
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


/// Formatuje wartość do czytelnej postaci tekstowej (z kolorowaniem).
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

/// Zaawansowane formatowanie liczby DualValue.
/// Porównuje wartość binarną z precyzyjną i koloruje różnice na czerwoną.
/// Służy do wizualizacji błędów zmiennoprzecinkowych.
pub fn format_dual(val: DualValue) -> String {
    if val.binary.is_infinite() {
        let text = if val.binary.is_sign_positive() { "inf" } else { "-inf" };
        return text.red().to_string();
    }
    if val.binary.is_nan() { return "NaN".red().to_string(); }

    // Konwersja na stringi
    let b_raw = val.binary.to_string();
    let b_str = if b_raw.contains('.') {
        let t = b_raw.trim_end_matches('0').trim_end_matches('.');
        if t.is_empty() { "0" } else { t }
    } else {
        &b_raw
    };

    let p_raw = format!("{:.1000}", val.precise);
    let p_str = if p_raw.contains('.') {
        let t = p_raw.trim_end_matches('0').trim_end_matches('.');
        if t.is_empty() { "0" } else { t }
    } else {
        &p_raw
    };

    // ZASADA 2: Inna długość całkowita -> CAŁOŚĆ CZERWONA (znaczący błąd)
    let get_int_len = |s: &str| s.split('.').next().unwrap_or("").len();
    if get_int_len(b_str) != get_int_len(p_str) {
        return b_str.red().to_string();
    }

    let b_chars: Vec<char> = b_str.chars().collect();
    let p_chars: Vec<char> = p_str.chars().collect();
    
    // HEURYSTYKA:
    // Jeśli precise jest "naturalnie" nieskończone (>250 znaków), ucinamy.
    let is_implicit_precision = p_chars.len() > 250; 
    
    let display_limit = if is_implicit_precision {
        b_chars.len()
    } else {
        // Dla f100d pokazujemy tyle ile ma precise
        std::cmp::max(b_chars.len(), p_chars.len())
    };

    let mut result = String::new();
    let mut mismatch = false;

    for i in 0..display_limit {
        let bc = if i < b_chars.len() { Some(b_chars[i]) } else { None };
        let pc = if i < p_chars.len() { Some(p_chars[i]) } else { None };

        // Jeśli binary się skończyło, wyświetlamy precise (dla f100d)
        let char_to_print = bc.or(pc).unwrap_or('?');

        if mismatch {
            // ZASADA 1: Po wykryciu błędu reszta na czerwono
            result.push_str(&char_to_print.to_string().red().to_string());
        } else {
            match (bc, pc) {
                (Some(b), Some(p)) => {
                    // Mamy obie cyfry - sprawdzamy zgodność
                    if b != p {
                        mismatch = true;
                        result.push_str(&b.to_string().red().to_string());
                    } else {
                        // Zgodne - sprawdzamy czy to koniec widoku
                        let is_last_visible = i == display_limit - 1;
                        let is_truncated = p_chars.len() > display_limit;
                        
                        // ZASADA 3: Żółty koniec tylko jeśli ucięliśmy (w pamięci jest więcej)
                        if is_last_visible && is_truncated {
                            result.push_str(&b.to_string().yellow().to_string());
                        } else {
                            result.push(b);
                        }
                    }
                },
                (None, Some(p)) => {
                    // Binary się skończyło, ale Precise trwa (przypadek f100d).
                    // Nie błąd tylko precyzja rozszerzona.
                    // Wyświetlamy na biało (default).
                    result.push(p);
                },
                (Some(b), None) => {
                    // Binary ma cyfry, a Precise się skończyło? (Rzadkie, błąd float)
                    mismatch = true;
                    result.push_str(&b.to_string().red().to_string());
                },
                (None, None) => {}
            }
        }
    }
    
    result
}

// --- LOGIKA WYKONAWCZA (STMT) ---

/// Wykonuje pojedynczą instrukcję.
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
                
                // Obsługa ujemnych indeksów (liczenie od końca)
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

                // Warunek stopu pętli
                if *down { if curr_val.binary < e_val.binary { break; } } 
                else { if curr_val.binary > e_val.binary { break; } }

                let res = eval_stmt(body, env, inc)?;
                match res {
                    ExecResult::Return(v) => return Ok(ExecResult::Return(v)),
                    ExecResult::Next(v) => if v.is_some() { last = v; }
                }

                // Aktualizacja licznika pętli (inkrementacja/dekrementacja DualValue)
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

        Stmt::Include(raw_path) | Stmt::IncludeOnce(raw_path) => {
            // Logika wyszukiwania plików (folder std, rozszerzenia .pwo, .rs)
            let mut path_buf = if raw_path == "std" {
                std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src").join("std")
            } else {
                std::path::PathBuf::from(raw_path)
            };

            if !path_buf.exists() {
                let try_pwo = path_buf.with_extension("pwo");
                let try_rs = path_buf.with_extension("rs");
                
                if try_pwo.exists() {
                    path_buf = try_pwo;
                } else if try_rs.exists() {
                    path_buf = try_rs;
                }
            }

            let final_path = path_buf.to_string_lossy().to_string();

            if let Stmt::IncludeOnce(_) = stmt {
                if inc.contains(&final_path) { return Ok(ExecResult::Next(None)); }
                inc.insert(final_path);
            }

            let content = fs::read_to_string(&path_buf)
                .map_err(|e| format!("Błąd include '{}' (szukano: {:?}): {}", raw_path, path_buf, e))?;

            // Parsowanie i interpretacja dołączonego pliku
            let tokens = crate::lexer::tokenize(&content)?;
            let ast = crate::parser::parse(tokens)?;
            
            for s in ast {
                match eval_stmt(&s, env, inc)? {
                    ExecResult::Return(v) => return Ok(ExecResult::Return(v)),
                    ExecResult::Next(_) => {}
                }
            }
            Ok(ExecResult::Next(None))
        }
    }
}

// --- LOGIKA WYRAŻEŃ (EXPR) ---

/// Oblicza wartość wyrażenia.
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

            // Tworzenie nowego środowiska dla wywołania funkcji (domknięcie)
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
                    
                    // Operacje wykonywane równolegle na obu reprezentacjach (precise i binary)
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

/// Aplikuje rzutowanie typu na wartość DualValue.
/// Modyfikuje `binary` lub `precise` w zależności od trybu rzutowania.
fn apply_cast(val: DualValue, spec: &CastSpec) -> Result<DualValue, String> {
    let mut num_bin = val.binary;
    
    // Wczytujemy oryginał.
    // Czy go nadpiszemy? Zależy od trybu.
    let mut num_prec = val.precise; 

    match spec.mode {
        CastMode::Decimal => {
            // TRYB DECIMAL (np. f30d):
            // Użytkownik żąda konkretnej precyzji matematycznej.
            // MUSIMY zmodyfikować num_prec, aby liczba "stała się" 30-cyfrowa.
            match spec.cast_type {
                CastType::Float => {
                    let dp = spec.size as i64;
                    // Tu zmieniamy oryginał, bo takie jest życzenie użytkownika (chce liczbę o precyzji 30)
                    num_prec = num_prec.with_scale(dp);
                    // Binary jest tylko przybliżeniem tej nowej liczby
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
            // TRYB BITS (np. f32, u16):
            // Symulujemy rzutowanie sprzętowe.
            // NIE WOLNO zmieniać num_prec (zachowujemy idealny wzorzec).
            // Psujemy tylko num_bin.
            match spec.cast_type {
                CastType::Float => {
                    match spec.size {
                        64 => { },
                        32 => { num_bin = (num_bin as f32) as f64; },
                        16 => {
                            let mut f_bits = (num_bin as f32).to_bits();
                            f_bits &= 0xFFFFE000; // Maska dla f16
                            num_bin = f32::from_bits(f_bits) as f64;
                        },
                        8 => {
                            let mut f_bits = (num_bin as f32).to_bits();
                            f_bits &= 0xFFC00000; // Maska dla f8 (symulacja)
                            num_bin = f32::from_bits(f_bits) as f64;
                        },
                        _ => return Err(format!("Bity float: f64, f32, f16, f8. Otrzymano: f{}", spec.size)),
                    }
                },
                CastType::Int | CastType::Uint => {
                    let range_f = 2f64.powf(spec.size as f64);
                    let range = BigDecimal::from_f64(range_f).unwrap_or(BigDecimal::from(1));
                    
                    // W rzutowaniu bitowym na Int (wrap) matematyczna wartość też się zmienia (modulo),
                    // więc tu aktualizacja num_prec jest poprawna.
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

/// Pomocnicza funkcja wyciągająca DualValue z Value.
fn get_number_dv(v: Value) -> Result<DualValue, String> { 
    match v { Value::Number(dv) => Ok(dv), _ => Err("Błąd typu: Oczekiwano liczby".into()) } 
}

/// Pomocnicza funkcja wyciągająca bool z Value.
fn get_bool(v: Value) -> Result<bool, String> { 
    match v { Value::Bool(b) => Ok(b), _ => Err("Błąd typu: Oczekiwano Boola".into()) } 
}

/// Pomocnicza funkcja wyciągająca indeks listy (i64) z Value.
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