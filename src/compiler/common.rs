use crate::parser::parse::Parser;

pub trait Compiler {
	fn new(ast: Parser) -> Self
	where
		Self: Sized;
	fn compile(&mut self) -> String;
}
