use clap::Parser as _;

mod cli;

use mangrove_rs::compiler::common::Compiler;
use mangrove_rs::compiler::riscv::RiscVCompiler;
use mangrove_rs::compiler::wasm::WasmCompiler;
use mangrove_rs::optimiser::Optimiser;
use mangrove_rs::parser::{parse::Parser, tokeniser::Tokeniser};

use crate::cli::TargetArch;

fn compiler(target: TargetArch, parser: Parser) -> Box<dyn Compiler> {
	match target {
		cli::TargetArch::Wasm => Box::new(WasmCompiler::new(parser)),
		cli::TargetArch::Riscv => Box::new(RiscVCompiler::new(parser)),
	}
}

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
	let mut compiler = compiler(args.target(), parser);
	println!("{}", compiler.compile());
}
