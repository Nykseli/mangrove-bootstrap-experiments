use std::env::args;

use mangrove_rs::compiler::Compiler;
use mangrove_rs::parser::{parse::Parser, tokeniser::Tokeniser};

fn main() {
	let args: Vec<String> = args().collect();
	let tokeniser = Tokeniser::new(std::fs::read_to_string(&args[1]).unwrap());
	let mut parser = Parser::new(tokeniser);
	let ast = parser.parse();
	let mut compiler = Compiler::new(ast);
	println!("{}", compiler.compile());
}
