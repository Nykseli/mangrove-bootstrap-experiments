use crate::ast::{
	ASTAdd, ASTAssignArg, ASTAssignment, ASTAssignmentExpr, ASTBlock, ASTBlockStatement,
	ASTFunction, ASTFunctionCall, ASTFunctionCallArg,
};

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

	fn skip_white_peek(&mut self) -> Result<Token, ()> {
		let mut peek_idx = 1;
		let mut next = self.lexer.peek_token(peek_idx).unwrap();
		while next.type_() == TokenType::Whitespace || next.type_() == TokenType::Newline {
			next = self.lexer.peek_token(peek_idx).unwrap();
			peek_idx += 1;
		}

		Ok(next)
	}

	fn parse_function_call(&mut self, name: &str) -> ASTFunctionCall {
		// left paren is handled by parse_block for now
		/* let left_paren = self.skip_white().unwrap();
		if left_paren.type_() != TokenType::LeftParen {
			panic!("Expected left paren {left_paren:#?}");
		} */

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
				TokenType::Ident => args.push(ASTFunctionCallArg::Ident(arg.value().into())),
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

	fn parse_assignment(&mut self, type_token: Token, ident: Token) -> ASTAssignment {
		let assign = self.skip_white().unwrap();
		if assign.type_() != TokenType::AssignOp {
			panic!("Expected assign Op {assign:#?}");
		}

		let value = self.skip_white().unwrap();
		let value = match value.type_() {
			TokenType::IntLit => ASTAssignArg::Int32(value.value().parse::<i32>().unwrap()),
			TokenType::Ident => ASTAssignArg::Ident(value.value().into()),
			_ => unimplemented!("{value:#?}"),
		};

		let peek = self.skip_white_peek().unwrap();
		let expr = match peek.type_() {
			TokenType::AddOp => {
				// Skip the OpToken
				self.skip_white().unwrap();
				// get the assignment
				let rhs = self.skip_white().unwrap();
				let rhs = match rhs.type_() {
					TokenType::IntLit => ASTAssignArg::Int32(rhs.value().parse::<i32>().unwrap()),
					TokenType::Ident => ASTAssignArg::Ident(rhs.value().into()),
					_ => unimplemented!("{rhs:#?}"),
				};
				ASTAssignmentExpr::Add(ASTAdd { lhs: value, rhs })
			}
			_ => ASTAssignmentExpr::Arg(value),
		};

		ASTAssignment {
			expr,
			type_name: type_token.value().into(),
			ident: ident.value().into(),
		}
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

			let next = self.skip_white().unwrap();
			if statement.type_() == TokenType::Ident {
				if next.type_() == TokenType::LeftParen {
					let fn_call = self.parse_function_call(statement.value());
					statements.push(ASTBlockStatement::FunctionCall(fn_call));
				} else if next.type_() == TokenType::Ident {
					// TODO: Also parse the type
					let assign = self.parse_assignment(statement, next);
					statements.push(ASTBlockStatement::Assignment(assign))
				} else {
					panic!("Expected left paren or identifier")
				}
			};

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
			panic!("Expected left paren {left_paren:#?}");
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
