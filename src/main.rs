mod ast;
mod lexer;
mod parser;
mod interp;

use std::io::{self, Write};
use std::env;
use std::fs;
use colored::*; // Używamy biblioteki colored dla spójności z interp.rs

fn execute(source: &str, interpreter: &mut interp::Interpreter) -> Result<interp::Value, String> {
    let tokens = lexer::tokenize(source)?;
    let ast = parser::parse(tokens)?;
    interpreter.interpret(ast)
}

fn main() {
    // Wymuszenie kolorów na Windows
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
                    Ok(result) => {
                        // Nie narzucamy koloru na 'result', bo format_value sam decyduje (np. czerwony błąd)
                        println!("{} {}", ">>".green(), interp::format_value(result));
                    },
                    Err(e) => eprintln!("{} {}", "Runtime Error:".red().bold(), e),
                }
            }
            Err(e) => eprintln!("{} '{}': {}", "Błąd odczytu pliku".red(), filename, e),
        }
    } else {
        // --- TRYB INTERAKTYWNY (REPL) ---
        println!("{} v0.8 (Lists, Recursion & Casting)", "Interpreter".bold());
        println!("Wpisz 'exit', aby wyjść.");
        
        loop {
            // Znak zachęty
            print!("{} ", ">".blue().bold());
            io::stdout().flush().unwrap();

            let mut input = String::new();
            if io::stdin().read_line(&mut input).is_err() { break; }
            let input = input.trim();
            
            if input.eq_ignore_ascii_case("exit") { break; }
            if input.is_empty() { continue; }

            match execute(input, &mut interpreter) {
                Ok(result) => {
                    // Wyświetlamy ">>" na zielono, a wynik w jego własnym kolorze
                    // (dzięki temu błędy precyzji w liczbach będą czerwone)
                    println!("{} {}", ">>".green(), interp::format_value(result));
                },
                Err(e) => println!("{} {}", "Error:".red(), e),
            }
        }
    }
}