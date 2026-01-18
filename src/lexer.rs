// Zmieniona linijka: tylko Debug, Clone, PartialEq
#[derive(Debug, Clone, PartialEq)] 
pub enum Token {
    Number(f64),
    Ident(String),
    
    // Operatory
    Plus, Minus, Star, Slash, Caret, // ^
    Assign, Eq, // :=, =
    
    // Nawiasy
    LParen, RParen, LBracket, RBracket, // ( ) [ ]
    Semi,
    
    // Słowa kluczowe
    If, Then, Else,
    For, From, To, Downto, Do, Od, End,
    Return,
    
    Eof,
}

// 1. Ręczna implementacja Eq (wymagana przez Hash)
// Oszukujemy Rusta, obiecując, że nie będziemy używać NaN w kluczach mapy
impl Eq for Token {}

// 2. Ręczna implementacja Hash (bez zmian logicznych, ale teraz jest jedyna)
impl std::hash::Hash for Token {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Token::Number(f) => f.to_bits().hash(state),
            Token::Ident(s) => s.hash(state),
            _ => {}
        }
    }
}

pub fn tokenize(input: &str) -> Result<Vec<Token>, String> {
    // ... reszta funkcji tokenize BEZ ZMIAN ...
    // (Wklej tutaj ciało funkcji tokenize z poprzedniej odpowiedzi)
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
                let n = num_str.parse::<f64>().map_err(|_| format!("Błąd liczby: {}", num_str))?;
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
                    "if" => Token::If, "then" => Token::Then, "else" => Token::Else,
                    "for" => Token::For, "from" => Token::From, "to" => Token::To,
                    "downto" => Token::Downto, "do" => Token::Do, "od" => Token::Od,
                    "end" => Token::End, "Return" => Token::Return,
                    _ => Token::Ident(ident),
                };
                tokens.push(token);
            }
            ':' => {
                chars.next();
                if let Some('=') = chars.peek() { chars.next(); tokens.push(Token::Assign); }
                else { return Err("Błąd składni: Oczekiwano '=' po ':'".into()); }
            }
            '^' => { tokens.push(Token::Caret); chars.next(); }
            '[' => { tokens.push(Token::LBracket); chars.next(); }
            ']' => { tokens.push(Token::RBracket); chars.next(); }
            '=' => { tokens.push(Token::Eq); chars.next(); }
            ';' => { tokens.push(Token::Semi); chars.next(); }
            '+' => { tokens.push(Token::Plus); chars.next(); }
            '-' => { tokens.push(Token::Minus); chars.next(); }
            '*' => { tokens.push(Token::Star); chars.next(); }
            '/' => { tokens.push(Token::Slash); chars.next(); }
            '(' => { tokens.push(Token::LParen); chars.next(); }
            ')' => { tokens.push(Token::RParen); chars.next(); }
            c if c.is_whitespace() => { chars.next(); }
            _ => return Err(format!("Nieznany znak: {}", c)),
        }
    }
    tokens.push(Token::Eof);
    Ok(tokens)
}