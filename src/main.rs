mod ast;
mod lexer;
mod parser;
mod interp;

use std::io::{self, Write};

fn main() {
    println!("=== Kalkulator Rust (f64) ===");
    println!("Wpisz wyrażenie (np. -2.5 * (3 + 4)) lub 'exit'");

    loop {
        print!("> ");
        // Flush jest konieczny, żeby tekst pojawił się przed pobraniem danych
        io::stdout().flush().unwrap();

        let mut input = String::new();
        if io::stdin().read_line(&mut input).is_err() {
            break;
        }

        let input = input.trim();
        if input == "exit" || input.is_empty() {
            break;
        }

        // Pipeline: Tekst -> Tokeny -> AST -> Wynik
        match lexer::tokenize(input) {
            Ok(tokens) => {
                match parser::parse(tokens) {
                    Ok(ast) => {
                        match interp::interpret(ast) {
                            Ok(val) => println!("= {}", val),
                            Err(e) => eprintln!("Runtime Error: {}", e),
                        }
                    },
                    Err(e) => eprintln!("Syntax Error: {}", e),
                }
            },
            Err(e) => eprintln!("Lexer Error: {}", e),
        }
    }
}