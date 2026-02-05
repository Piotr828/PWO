/// Reprezentuje operatory dostępne w języku.
///
/// Enum ten grupuje operatory arytmetyczne, logiczne oraz porównania.
#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    // Arytmetyka
    /// Dodawanie (`+`).
    Add,
    /// Odejmowanie (`-`).
    Sub,
    /// Mnożenie (`*`).
    Mul,
    /// Dzielenie (`/`).
    Div,
    /// Reszta z dzielenia (modulo, `%`).
    Mod,
    /// Potęgowanie (`^` lub `**`).
    Pow,

    // Logika
    /// Logiczne "i" (`&&`).
    And,
    /// Logiczne "lub" (`||`).
    Or,
    /// Logiczna negacja (`!`).
    Not,

    // Porównania
    /// Równość (`==`).
    Eq,
    /// Nierówność (`!=`).
    Neq,
    /// Mniejsze niż (`<`).
    Lt,
    /// Większe niż (`>`).
    Gt,
    /// Mniejsze lub równe (`<=`).
    Lte,
    /// Większe lub równe (`>=`).
    Gte,
}

/// Określa typ docelowy dla operacji rzutowania (castowania).
#[derive(Debug, Clone, PartialEq)]
pub enum CastType {
    /// Liczba całkowita ze znakiem (np. `i32`).
    Int,
    /// Liczba całkowita bez znaku (np. `u32`).
    Uint,
    /// Liczba zmiennoprzecinkowa (np. `f64`).
    Float,
}

/// Definiuje sposób, w jaki wartość ma zostać zinterpretowana podczas rzutowania.
#[derive(Debug, Clone, PartialEq)]
pub enum CastMode {
    /// Rzutowanie bitowe (reinterpretacja układu bitów).
    Bits,
    /// Rzutowanie wartości (konwersja numeryczna, np. `1.5` -> `1`).
    Decimal,
}

/// Pełna specyfikacja operacji rzutowania.
///
/// Struktura ta przechowuje wszystkie metadane potrzebne do wykonania konwersji typu.
#[derive(Debug, Clone, PartialEq)]
pub struct CastSpec {
    /// Docelowy typ danych.
    pub cast_type: CastType,
    /// Rozmiar typu w bitach (np. 32, 64).
    pub size: u32,
    /// Tryb konwersji (bitowy lub dziesiętny).
    pub mode: CastMode,
}

/// Reprezentuje wyrażenie (expression), które ewaluuje się do konkretnej wartości.
///
/// Jest to główny element drzewa składniowego (AST) odpowiedzialny za obliczenia.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// Literał liczbowy zmiennoprzecinkowy (domyślnie `f64`).
    Number(f64),
    /// Literał logiczny (`true` / `false`).
    Bool(bool),
    /// Odwołanie do zmiennej przez jej nazwę.
    Variable(String),
    /// Operacja binarna (dwuargumentowa), np. `a + b`.
    Binary(Box<Expr>, Op, Box<Expr>),
    /// Operacja unarna (jednoargumentowa), np. `-a` lub `!a`.
    Unary(Op, Box<Expr>),
    /// Pobranie elementu z listy za pomocą indeksu: `lista[indeks]`.
    ListGet(String, Box<Expr>),

    /// Jawne rzutowanie typu wyrażenia na inny typ.
    Cast(CastSpec, Box<Expr>),

    /// Literał listy, tworzący nową wektor wartości, np. `[1, 2, 3]`.
    ListLiteral(Vec<Expr>),

    /// Wywołanie funkcji lub lambdy z listą argumentów.
    Call(Box<Expr>, Vec<Expr>),
    /// Definicja funkcji anonimowej (lambda).
    ///
    /// * `Vec<String>` - Lista nazw argumentów.
    /// * `Box<Stmt>` - Ciało funkcji (zazwyczaj blok instrukcji).
    Lambda(Vec<String>, Box<Stmt>),
}

/// Reprezentuje instrukcję (statement), która wykonuje akcję, ale niekoniecznie zwraca wartość.
///
/// Instrukcje sterują przepływem programu (pętle, warunki) oraz efektami ubocznymi (przypisania).
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    /// Przypisanie wartości do zmiennej: `x = 5`.
    Assignment(String, Expr),
    /// Przypisanie wartości do elementu listy: `tablica[i] = 5`.
    ListAssignment(String, Expr, Expr),
    /// Blok kodu grupujący wiele instrukcji (nowy zakres zmiennych).
    Block(Vec<Stmt>),

    /// Instrukcja warunkowa `if-else`.
    If {
        /// Warunek logiczny.
        cond: Expr,
        /// Blok wykonywany, gdy warunek jest prawdziwy.
        then_body: Box<Stmt>,
        /// Opcjonalny blok `else`, wykonywany gdy warunek jest fałszywy.
        else_body: Option<Box<Stmt>>,
    },
    /// Pętla numeryczna `for`.
    For {
        /// Nazwa zmiennej iteracyjnej.
        var: String,
        /// Wartość początkowa.
        start: Expr,
        /// Wartość końcowa (granica).
        end: Expr,
        /// Czy pętla odlicza w dół (`true`), czy w górę (`false`).
        down: bool,
        /// Ciało pętli.
        body: Box<Stmt>,
    },
    /// Pętla warunkowa `while`.
    While {
        /// Warunek kontynuacji pętli.
        cond: Expr,
        /// Ciało pętli.
        body: Box<Stmt>,
    },

    /// Definicja funkcji nazwanej.
    ///
    /// * Argument 1: Nazwa funkcji.
    /// * Argument 2: Lista nazw parametrów.
    /// * Argument 3: Ciało funkcji.
    Function(String, Vec<String>, Box<Stmt>),

    /// Załączenie innego pliku źródłowego.
    Include(String),
    /// Załączenie pliku źródłowego tylko raz (unikając duplikatów).
    IncludeOnce(String),

    /// Zwrócenie wartości z funkcji i zakończenie jej wykonywania.
    Return(Expr),
    /// Instrukcja będąca pojedynczym wyrażeniem (np. wywołanie funkcji typu void).
    Expr(Expr),
}