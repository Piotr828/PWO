/// Reprezentuje pojedynczy token (jednostkę leksykalną) w kodzie źródłowym.
///
/// Tokeny są "atomami", z których parser buduje drzewo składniowe (AST).
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Literały i Identyfikatory
    /// Liczba zmiennoprzecinkowa (wszystkie liczby w tym języku są `f64`).
    Number(f64),
    /// Nazwa zmiennej lub funkcji.
    Ident(String),

    // Operatory Arytmetyczne i Logiczne
    Plus, Minus, Star, Slash, Mod, Caret,
    And, Or, Not,
    Eq, Neq,
    Lt, Gt, Lte, Gte,

    // Znaki Interpunkcyjne i Specjalne
    Arrow,    // ->
    Pipe,     // |
    Comma,    // ,

    Assign,   // :=
    LParen, RParen, LBracket, RBracket,
    Semi,     // ;

    /// Specjalny token rzutowania typu.
    ///
    /// Wykrywany, gdy napotkamy konstrukcję typu `(i32)` lub `(f64d)`.
    /// * `char` - Typ podstawowy ('i', 'u', 'f').
    /// * `u32` - Rozmiar w bitach (np. 32, 64).
    /// * `bool` - Flaga trybu dziesiętnego (`true` jeśli suffix 'd').
    Cast(char, u32, bool),

    // Słowa Kluczowe (Keywords)
    If, Then, Else, Fi,
    True, False,
    For, From, To, Downto, Do, Od, End,
    While,
    Fn,
    Include, IncludeOnce,
    Return,

    /// Koniec pliku / strumienia tokenów.
    Eof,
}

// Implementacja `Eq` jest wymagana, jeśli chcemy używać Tokenów jako kluczy w mapach,
// mimo że `f64` domyślnie nie implementuje `Eq` (przez NaN).
impl Eq for Token {}

/// Implementacja haszowania dla Tokenów.
///
/// Jest to konieczne, ponieważ `f64` nie implementuje `Hash` standardowo.
/// Używamy binarnej reprezentacji liczby (`to_bits`) do obliczenia skrótu.
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



/// Przekształca surowy ciąg znaków (kod źródłowy) w wektor tokenów.
///
/// Funkcja iteruje po znakach, grupując je w logiczne całość (liczby, słowa kluczowe, operatory).
/// Obsługuje również pomijanie białych znaków oraz komentarzy (`//` oraz `/* */`).
///
/// # Arguments
///
/// * `input` - Kod źródłowy jako `&str`.
///
/// # Returns
///
/// * `Ok(Vec<Token>)` - Lista tokenów zakończona `Token::Eof`.
/// * `Err(String)` - Komunikat błędu w przypadku napotkania nieznanego znaku lub błędu składni.
pub fn tokenize(input: &str) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    // Używamy `peekable`, aby móc "podglądać" następny znak bez konsumowania go.
    let mut chars = input.chars().peekable();

    while let Some(&c) = chars.peek() {
        match c {
            // Parsowanie Liczb
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

            // Parsowanie Identyfikatorów i Słów Kluczowych
            'a'..='z' | 'A'..='Z' | '_' => {
                let mut ident = String::new();
                while let Some(&ch) = chars.peek() {
                    if ch.is_ascii_alphanumeric() || ch == '_' {
                        ident.push(ch); chars.next();
                    } else { break; }
                }
                // Sprawdzenie, czy identyfikator jest słowem kluczowym
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
                    "use" => Token::Include,
                    "include_once" => Token::IncludeOnce,
                    "use1" => Token::IncludeOnce,
                    _ => Token::Ident(ident),
                };
                tokens.push(token);
            }

            // Operatory Wieloznakowe
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

            // Komentarze i Dzielenie
            '/' => {
                chars.next();
                match chars.peek() {
                    Some('/') => { // Komentarz jednolinijkowy
                        while let Some(&ch) = chars.peek() {
                            if ch != '\n' { chars.next(); } else { break; }
                        }
                    },
                    Some('*') => { // Komentarz blokowy
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
                    _ => tokens.push(Token::Slash), // Zwykłe dzielenie
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

            // Wykrywanie Rzutowania (Cast) vs Nawias
            '(' => {
                // Klonujemy iterator, aby bezpiecznie "zajrzeć w przyszłość"
                // bez modyfikowania głównego licznika, jeśli to nie jest cast.
                let mut lookahead = chars.clone();
                lookahead.next(); // Pomiń '('

                let mut is_cast = false;

                // Sprawdzamy czy wzorzec pasuje do (i.. | u.. | f..)
                if let Some(type_char @ ('i' | 'u' | 'f')) = lookahead.next() {
                    let mut num_str = String::new();

                    // Czytamy cyfry rozmiaru (np. 32, 64)
                    while let Some(&d) = lookahead.peek() {
                        if d.is_ascii_digit() {
                            num_str.push(d);
                            lookahead.next();
                        } else { break; }
                    }

                    if !num_str.is_empty() {
                        // Sprawdzamy opcjonalny sufiks 'd' (decimal mode)
                        let is_decimal = if let Some('d') = lookahead.peek() {
                            lookahead.next(); true
                        } else { false };

                        // Jeśli na końcu jest ')' to mamy pełny cast!
                        if let Some(')') = lookahead.peek() {
                            lookahead.next();
                            chars = lookahead; // Zatwierdzamy przesunięcie iteratora
                            
                            let size = num_str.parse::<u32>().unwrap_or(0);
                            tokens.push(Token::Cast(type_char, size, is_decimal));
                            is_cast = true;
                        }
                    }
                }

                if !is_cast {
                    // To nie był cast, traktujemy jako zwykły nawias
                    tokens.push(Token::LParen);
                    chars.next();
                }
            }

            // Pojedyncze Znaki
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