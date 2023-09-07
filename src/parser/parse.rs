use crate::ast::{ASTBlock, ASTFunction, ASTFunctionCall, ASTFunctionCallArg};

use super::tokeniser::Tokeniser;
use super::types::{Token, TokenType};

pub struct Parser {
	lexer: Tokeniser,
}

impl Parser {
	pub fn new(lexer: Tokeniser) -> Self {
		Self { lexer }
	}

	fn skip_white(&mut self) -> Result<Token, ()> {
		let mut next = self.lexer.next_token().unwrap();
		while next.type_() == TokenType::Whitespace || next.type_() == TokenType::Newline {
			next = self.lexer.next_token().unwrap();
		}
		Ok(next)
	}

	fn parse_function_call(&mut self, name: &str) -> ASTFunctionCall {
		let left_paren = self.skip_white().unwrap();
		if left_paren.type_() != TokenType::LeftParen {
			panic!("Expected left paren");
		}

		let mut args = Vec::<ASTFunctionCallArg>::new();
		let mut arg = self.skip_white().unwrap();
		while arg.type_() != TokenType::RightParen {
			match arg.type_() {
				TokenType::StringLit => args.push(ASTFunctionCallArg::String(arg.value().into())),
				TokenType::CharLit => args.push(ASTFunctionCallArg::Char(
					arg.value().chars().next().unwrap(),
				)),
				TokenType::IntLit => {
					args.push(ASTFunctionCallArg::Int32(arg.value().parse().unwrap()))
				}
				TokenType::Comma => (),
				_ => unimplemented!("{arg:#?}"),
			}
			arg = self.skip_white().unwrap();
		}

		if arg.type_() != TokenType::RightParen {
			panic!("Expected right paren");
		}

		ASTFunctionCall::new(name.into(), args)
	}

	fn parse_block(&mut self) -> ASTBlock {
		let left_brace = self.skip_white().unwrap();
		if left_brace.type_() != TokenType::LeftBrace {
			panic!("Expected left brace");
		}

		let mut statements = Vec::new();
		let mut statement = self.skip_white().unwrap();
		loop {
			if statement.type_() == TokenType::RightBrace {
				break;
			}

			if statement.type_() == TokenType::Ident {
				statements.push(self.parse_function_call(statement.value()))
			}

			statement = self.skip_white().unwrap();
		}

		ASTBlock::new(statements)
	}

	fn parse_function(&mut self) -> ASTFunction {
		let ident = self.skip_white().unwrap();
		let name = if ident.type_() == TokenType::Ident {
			ident.value()
		} else {
			panic!("Expected identifier after 'function'")
		};

		let left_paren = self.skip_white().unwrap();
		if left_paren.type_() != TokenType::LeftParen {
			panic!("Expected left paren");
		}

		let right_paren = self.skip_white().unwrap();
		if right_paren.type_() != TokenType::RightParen {
			panic!("Expected right paren");
		}

		let body = self.parse_block();

		ASTFunction::new(name.into(), body)
	}

	pub fn parse(&mut self) -> Vec<ASTFunction> {
		let mut nodes = Vec::new();
		let mut current = self.skip_white();

		while let Ok(token) = current {
			if token.type_() == TokenType::Eof {
				break;
			} else if token.type_() == TokenType::FunctionDef {
				nodes.push(self.parse_function());
			} else {
				unimplemented!("Only FunctionDef statements can be parsed!")
			}

			current = self.skip_white();
		}

		nodes
	}
}
