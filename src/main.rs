mod ast;
mod lexer;
mod parser;
mod interp;

use std::io::{self, Write};
use std::env;
use std::fs;
use colored::*; // Używamy biblioteki colored dla spójności z interp.rs

/// Przeprowadza pełny proces wykonania kodu źródłowego.
///
/// Funkcja ta łączy trzy główne etapy interpretacji języka:
/// 1. **Tokenizacja** (`lexer::tokenize`) - zamiana tekstu na ciąg symboli.
/// 2. **Parsowanie** (`parser::parse`) - budowa drzewa składniowego (AST).
/// 3. **Interpretacja** (`interpreter.interpret`) - wykonanie logiki programu.
///
/// # Arguments
///
/// * `source` - Kod źródłowy jako ciąg znaków.
/// * `interpreter` - Mutowalna referencja do instancji interpretera (przechowuje stan/zmienne).
///
/// # Returns
///
/// Zwraca obliczoną wartość (`interp::Value`) lub komunikat błędu (`String`).
fn execute(source: &str, interpreter: &mut interp::Interpreter) -> Result<interp::Value, String> {
    let tokens = lexer::tokenize(source)?;
    let ast = parser::parse(tokens)?;
    interpreter.interpret(ast)
}

/// Główny punkt wejścia do aplikacji.
///
/// Obsługuje dwa tryby działania w zależności od argumentów wiersza poleceń:
/// 1. **Tryb wsadowy (File Mode):** Uruchamiany, gdy podano ścieżkę do pliku jako argument.
///    Wczytuje plik, wykonuje go i kończy działanie.
/// 2. **Tryb interaktywny (REPL):** Uruchamiany bez argumentów.
///    Umożliwia wpisywanie komend "na żywo" w pętli Read-Eval-Print Loop.
fn main() {
    // Wymuszenie obsługi kodów ANSI (kolorów) na systemie Windows 10/11.
    // Bez tego kolory z biblioteki `colored` mogłyby wyświetlać się jako dziwne znaki.
    #[cfg(windows)]
    let _ = colored::control::set_virtual_terminal(true);

    let args: Vec<String> = env::args().collect();
    let mut interpreter = interp::Interpreter::new();

    if args.len() > 1 {
        // TRYB PLIKU
        let filename = &args[1];
        match fs::read_to_string(filename) {
            Ok(content) => {
                match execute(&content, &mut interpreter) {
                    Ok(result) => {
                        // Nie narzucamy koloru na 'result', bo format_value sam decyduje (np. czerwony błąd precyzji)
                        println!("{} {}", ">>".green(), interp::format_value(result));
                    },
                    // Błędy wykonania (Runtime Error) wypisujemy na stderr.
                    Err(e) => eprintln!("{} {}", "Runtime Error:".red().bold(), e),
                }
            }
            Err(e) => eprintln!("{} '{}': {}", "Błąd odczytu pliku".red(), filename, e),
        }
    } else {
        // TRYB INTERAKTYWNY (REPL)
        println!("{}", "Interpreter (REPL)".bold());
        println!("Wpisz 'exit', aby wyjść.");
        
        loop {
            // Wyświetlenie znaku zachęty (prompt)
            print!("{} ", ">".blue().bold());
            // Wymuszenie wypisania bufora (niezbędne, bo `print!` nie dodaje nowej linii)
            io::stdout().flush().unwrap();

            let mut input = String::new();
            if io::stdin().read_line(&mut input).is_err() { break; }
            let input = input.trim();
            
            // Obsługa wyjścia z REPL
            if input.eq_ignore_ascii_case("exit") { break; }
            if input.is_empty() { continue; }

            match execute(input, &mut interpreter) {
                Ok(result) => {
                    // Wyświetlamy ">>" na zielono, a wynik w jego własnym kolorze.
                    // Dzięki temu błędy precyzji w liczbach (DualValue) będą widoczne jako czerwone fragmenty.
                    println!("{} {}", ">>".green(), interp::format_value(result));
                },
                Err(e) => println!("{} {}", "Error:".red(), e),
            }
        }
    }
}