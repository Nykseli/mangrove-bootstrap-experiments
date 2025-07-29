use crate::parser::parse::Parser;

pub trait ArchCompiler {
	fn new(ast: Parser) -> Self;
	fn compile(&mut self) -> String;
}

pub struct Compiler<T> {
	inner: T,
}

impl<T: ArchCompiler> Compiler<T> {
	pub fn new(ast: Parser) -> Self {
		Self { inner: T::new(ast) }
	}

	pub fn compile(&mut self) -> String {
		self.inner.compile()
	}
}
