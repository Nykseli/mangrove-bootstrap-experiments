use crate::ast::{
	ASTAdd, ASTAssignArg, ASTAssignment, ASTAssignmentExpr, ASTBlock, ASTBlockStatement,
	ASTFunction, ASTFunctionCall, ASTFunctionCallArg, ASTReturn,
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

	fn parse_function_call(&mut self, name: &str, used: bool) -> ASTFunctionCall {
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

		ASTFunctionCall::new(name.into(), args, used)
	}

	fn parse_assign_expr(&mut self) -> ASTAssignmentExpr {
		let value_token = self.skip_white().unwrap();
		let value = match value_token.type_() {
			TokenType::IntLit => ASTAssignArg::Int32(value_token.value().parse::<i32>().unwrap()),
			TokenType::Ident => ASTAssignArg::Ident(value_token.value().into()),
			_ => unimplemented!("{value_token:#?}"),
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
			TokenType::LeftParen => {
				// Skip the LeftParen
				self.skip_white().unwrap();
				// TODO: args
				let right_paren = self.skip_white().unwrap();
				if right_paren.type_() != TokenType::RightParen {
					panic!("Expected right paren");
				}
				ASTAssignmentExpr::FunctionCall(ASTFunctionCall::new(
					value_token.value().into(),
					vec![],
					true,
				))
			}
			_ => ASTAssignmentExpr::Arg(value),
		};

		expr
	}

	fn parse_assignment(&mut self, type_token: Token, ident: Token) -> ASTAssignment {
		let assign = self.skip_white().unwrap();
		if assign.type_() != TokenType::AssignOp {
			panic!("Expected assign Op {assign:#?}");
		}

		let expr = self.parse_assign_expr();

		ASTAssignment {
			expr,
			type_name: type_token.value().into(),
			ident: ident.value().into(),
		}
	}

	fn parse_return_statement(&mut self) -> ASTReturn {
		let expr = self.parse_assign_expr();
		ASTReturn { expr }
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
				let next = self.skip_white().unwrap();
				if next.type_() == TokenType::LeftParen {
					let fn_call = self.parse_function_call(statement.value(), false);
					statements.push(ASTBlockStatement::FunctionCall(fn_call));
				} else if next.type_() == TokenType::Ident {
					// TODO: Also parse the type
					let assign = self.parse_assignment(statement, next);
					statements.push(ASTBlockStatement::Assignment(assign))
				} else {
					panic!("Expected left paren or identifier")
				}
			} else if statement.type_() == TokenType::ReturnStmt {
				let return_stmt = self.parse_return_statement();
				statements.push(ASTBlockStatement::Return(return_stmt))
			} else {
				panic!("Expected identifier got: {statement:#?}")
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

		let arrow = self.skip_white().unwrap();
		if arrow.type_() != TokenType::Arrow {
			panic!("Expected arrow");
		}

		let return_type = self.skip_white().unwrap();
		let returns = match return_type.type_() {
			TokenType::NoneType => false,
			/// Just assuming that it's Int32
			TokenType::Ident => true,
			_ => panic!("Didn't expect {return_type:#?}"),
		};

		let body = self.parse_block();

		ASTFunction::new(name.into(), body, returns)
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
