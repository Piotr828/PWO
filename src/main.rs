mod ast;
mod lexer;
mod parser;
mod interp;

use std::io::{self, Write};
use std::env;
use std::fs;

// Stałe kolory ANSI (ponieważ używasz ich bezpośrednio)
const GREEN: &str = "\x1b[32m";
const RED: &str = "\x1b[31m";
const BLUE: &str = "\x1b[34m";
const BOLD: &str = "\x1b[1m";
const RESET: &str = "\x1b[0m";

fn execute(source: &str, interpreter: &mut interp::Interpreter) -> Result<interp::Value, String> {
    let tokens = lexer::tokenize(source)?;
    let ast = parser::parse(tokens)?;
    interpreter.interpret(ast)
}

fn main() {
    // Wymuszenie kolorów na Windows (wymaga crate 'colored' w Cargo.toml)
    #[cfg(windows)]
    let _ = colored::control::set_virtual_terminal(true);

    let args: Vec<String> = env::args().collect();
    let mut interpreter = interp::Interpreter::new();

    if args.len() > 1 {
        // --- TRYB PLIKU ---
        let filename = &args[1];
        match fs::read_to_string(filename) {
            Ok(content) => {
                match execute(&content, &mut interpreter) {
                    // Wyświetlamy wynik tylko jeśli jest, formatujemy go funkcją z interp
                    Ok(result) => println!("{}>> {}{}", GREEN, interp::format_value(result), RESET),
                    Err(e) => eprintln!("{}Runtime Error: {}{}", RED, e, RESET),
                }
            }
            Err(e) => eprintln!("{}Błąd odczytu pliku '{}': {}{}", RED, filename, e, RESET),
        }
    } else {
        // --- TRYB INTERAKTYWNY (REPL) ---
        println!("{}Interpreter v0.7 (Functions & Loops) - Tryb REPL{}", BOLD, RESET);
        println!("Wpisz 'exit', aby wyjść.");
        
        loop {
            print!("{}> {}", BLUE, RESET);
            io::stdout().flush().unwrap();

            let mut input = String::new();
            if io::stdin().read_line(&mut input).is_err() { break; }
            let input = input.trim();
            
            if input.eq_ignore_ascii_case("exit") { break; }
            if input.is_empty() { continue; }

            match execute(input, &mut interpreter) {
                Ok(result) => println!("{}>> {}{}", GREEN, interp::format_value(result), RESET),
                Err(e) => println!("{}Error: {}{}", RED, e, RESET),
            }
        }
    }
}