use clap::Parser as _;

mod cli;

use mangrove_rs::compiler::common::Compiler;
use mangrove_rs::compiler::wasm::WasmCompiler;
use mangrove_rs::optimiser::Optimiser;
use mangrove_rs::parser::{parse::Parser, tokeniser::Tokeniser};

fn main() {
	let args = cli::Args::parse();
	let tokeniser = Tokeniser::new(std::fs::read_to_string(args.file()).unwrap());
	let mut parser = Parser::new(tokeniser);
	parser.parse();
	if args.optimise() {
		let mut optimiser = Optimiser::new(parser);
		optimiser.optimise();
		parser = optimiser.parser;
	}
	let mut compiler = Compiler::<WasmCompiler>::new(parser);
	println!("{}", compiler.compile());
}
