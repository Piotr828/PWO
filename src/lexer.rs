#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Number(f64),
    Ident(String),
    
    Plus, Minus, Star, Slash, Mod, Caret,
    And, Or, Not,
    Eq, Neq,
    Lt, Gt, Lte, Gte,
    
    Arrow,
    Pipe,
    Comma,

    Assign,
    LParen, RParen, LBracket, RBracket,
    Semi,
    Cast(char, u32, bool), 
    
   
    If, Then, Else, Fi,
    True, False,
    For, From, To, Downto, Do, Od, End,
    While,
    Fn,
    Include, IncludeOnce,
    Return,
    
    Eof,
}

impl Eq for Token {}

impl std::hash::Hash for Token {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Token::Number(f) => f.to_bits().hash(state),
            Token::Ident(s) => s.hash(state),
            Token::Cast(c, s, b) => {
                c.hash(state);
                s.hash(state);
                b.hash(state);
            }
            _ => {}
        }
    }
}

pub fn tokenize(input: &str) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(&c) = chars.peek() {
        match c {
            '0'..='9' | '.' => {
                let mut num_str = String::new();
                while let Some(&ch) = chars.peek() {
                    if ch.is_ascii_digit() || ch == '.' {
                        num_str.push(ch); chars.next();
                    } else { break; }
                }
                let n = num_str.parse::<f64>().map_err(|_| format!("Nieznana liczba: {}", num_str))?;
                tokens.push(Token::Number(n));
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let mut ident = String::new();
                while let Some(&ch) = chars.peek() {
                    if ch.is_ascii_alphanumeric() || ch == '_' {
                        ident.push(ch); chars.next();
                    } else { break; }
                }
                let token = match ident.as_str() {
                    "if" => Token::If, "then" => Token::Then, "else" => Token::Else, "fi" => Token::Fi,
                    "True" => Token::True, "False" => Token::False,
                    "for" => Token::For, "from" => Token::From, "to" => Token::To,
                    "downto" => Token::Downto, "do" => Token::Do, "od" => Token::Od,
                    "while" => Token::While,
                    "end" => Token::End, 
                    "Return" | "return" => Token::Return,
                    "fn" => Token::Fn,
                    "include" => Token::Include,
                    "include_once" => Token::IncludeOnce,
                    _ => Token::Ident(ident),
                };
                tokens.push(token);
            }
            ':' => {
                chars.next();
                if let Some('=') = chars.peek() { chars.next(); tokens.push(Token::Assign); }
                else { return Err("Błąd składni: Oczekiwano ':=' ".into()); }
            }
            '=' => { tokens.push(Token::Eq); chars.next(); }
            '&' => {
                chars.next();
                if let Some('&') = chars.peek() { chars.next(); tokens.push(Token::And); }
                else { return Err("Błąd: Spróbuj && zamiast &".into()); }
            }
            '|' => {
                chars.next();
                if let Some('|') = chars.peek() { 
                    chars.next(); 
                    tokens.push(Token::Or); 
                } else { 
                    tokens.push(Token::Pipe); 
                }
            }
            '!' => { 
                chars.next();
                if let Some('=') = chars.peek() { chars.next(); tokens.push(Token::Neq); }
                else { tokens.push(Token::Not); }
            }
            '<' => {
                chars.next();
                if let Some('=') = chars.peek() { chars.next(); tokens.push(Token::Lte); }
                else { tokens.push(Token::Lt); }
            }
            '>' => {
                chars.next();
                if let Some('=') = chars.peek() { chars.next(); tokens.push(Token::Gte); }
                else { tokens.push(Token::Gt); }
            }
            '/' => { 
                chars.next();
                match chars.peek() {
                    Some('/') => {
                        while let Some(&ch) = chars.peek() {
                            if ch != '\n' { chars.next(); } else { break; }
                        }
                    },
                    Some('*') => {
                        chars.next();
                        while let Some(ch) = chars.next() {
                            if ch == '*' {
                                if let Some('/') = chars.peek() {
                                    chars.next();
                                    break;
                                }
                            }
                        }
                    },
                    _ => tokens.push(Token::Slash),
                }
            }
            '-' => { 
                chars.next();
                if let Some('>') = chars.peek() {
                    chars.next();
                    tokens.push(Token::Arrow);
                } else {
                    tokens.push(Token::Minus);
                }
            }
           
            '(' => { 
               
                let mut lookahead = chars.clone();
                lookahead.next();
                
                let mut is_cast = false;
                
               
                if let Some(type_char @ ('i' | 'u' | 'f')) = lookahead.next() {
                    let mut num_str = String::new();
                   
                    while let Some(&d) = lookahead.peek() {
                        if d.is_ascii_digit() {
                            num_str.push(d);
                            lookahead.next();
                        } else { break; }
                    }
                    
                    if !num_str.is_empty() {
                       
                        let is_decimal = if let Some('d') = lookahead.peek() {
                            lookahead.next(); true
                        } else { false };
                        
                       
                        if let Some(')') = lookahead.peek() {
                            lookahead.next();
                            chars = lookahead; 
                            
                            let size = num_str.parse::<u32>().unwrap_or(0);
                            tokens.push(Token::Cast(type_char, size, is_decimal));
                            is_cast = true;
                        }
                    }
                }
                
                if !is_cast {
                   
                    tokens.push(Token::LParen); 
                    chars.next();
                }
            }
            ',' => { tokens.push(Token::Comma); chars.next(); }
            '%' => { tokens.push(Token::Mod); chars.next(); }
            '^' => { tokens.push(Token::Caret); chars.next(); }
            '[' => { tokens.push(Token::LBracket); chars.next(); }
            ']' => { tokens.push(Token::RBracket); chars.next(); }
            ';' => { tokens.push(Token::Semi); chars.next(); }
            '+' => { tokens.push(Token::Plus); chars.next(); }
            '*' => { tokens.push(Token::Star); chars.next(); }
            ')' => { tokens.push(Token::RParen); chars.next(); }
            c if c.is_whitespace() => { chars.next(); }
            _ => return Err(format!("Nieznany znak: {}", c)),
        }
    }
    tokens.push(Token::Eof);
    Ok(tokens)
}