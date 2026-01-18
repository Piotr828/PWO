mod ast;
mod lexer;
mod parser;
mod interp;

use std::env;
use std::fs;
use std::io::{self, Write};

fn main() {
    let args: Vec<String> = env::args().collect();
    
    // Tworzymy instancję interpretera (zawiera w sobie środowisko zmiennych)
    let mut interpreter = interp::Interpreter::new();

    if args.len() > 1 {
        let filename = &args[1];
        run_file(filename, &mut interpreter);
    } else {
        run_repl(&mut interpreter);
    }
}

fn run_file(path: &str, interpreter: &mut interp::Interpreter) {
    let source = fs::read_to_string(path).expect("Nie można otworzyć pliku");
    match execute(&source, interpreter) {
        Ok(res) => println!("Program zakończony wynikiem: {}", interp::format_dual(res)),
        Err(e) => eprintln!("Runtime Error: {}", e),
    }
}

fn run_repl(interpreter: &mut interp::Interpreter) {
    println!("Interpreter PWO v1.0 (wpisz 'exit' by wyjść)");
    println!("Legenda: Czerwone cyfry oznaczają błąd precyzji binarnej.");

    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        if io::stdin().read_line(&mut input).is_err() || input.trim() == "exit" {
            break;
        }
        if input.trim().is_empty() { continue; }
        
        match execute(&input, interpreter) {
            Ok(res) => println!("Wynik: {}", interp::format_dual(res)),
            Err(e) => println!("{}", e),
        }
    }
}

fn execute(source: &str, interpreter: &mut interp::Interpreter) -> Result<interp::DualValue, String> {
    let tokens = lexer::tokenize(source)?;
    let ast = parser::parse(tokens)?;
    
    // Wywołujemy metodę na obiekcie interpreter
    interpreter.interpret(ast)
}