mod ast;
mod interpreter;
mod lang;
mod scanner;

use clap::Parser;
use clap::ValueEnum;
use std::fs;
use std::path::PathBuf;

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    script: Option<PathBuf>,

    #[arg(value_enum, short, long)]
    mode: Mode,
    //TODO in the future options for interpreter or compiled
    //TODO in future GC options
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
enum Mode {
    AstWalking,
    Repl,
}

fn main() {
    let args = Args::parse();
    match args.mode {
        Mode::AstWalking => {
            //TODO better error messages in case script is missing
            let script = fs::read_to_string(args.script.unwrap()).unwrap();
            let tokens = scanner::Scanner::new(script.chars()).scan().unwrap();
            let tree = ast::Parser::new(tokens).parse().unwrap();
            interpreter::Interpreter::new().interprete(tree).unwrap();
        }
        _ => todo!(),
    }
}
