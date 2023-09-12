use crate::ast::{
	ASTAdd, ASTAssignArg, ASTAssignIdent, ASTAssignment, ASTAssignmentExpr, ASTBlock,
	ASTBlockStatement, ASTClass, ASTClassInit, ASTClassInitArg, ASTClassMember, ASTFunction,
	ASTFunctionCall, ASTFunctionCallArg, ASTIfStmt, ASTInt32Type, ASTLtStmt, ASTMinus, ASTReturn,
	ASTStaticAssign, ASTStringType, ASTType, ASTVariable, StaticValue,
};

use super::tokeniser::Tokeniser;
use super::types::{Token, TokenType};

#[derive(Debug)]
struct BlockCtx {
	variables: Vec<ASTVariable>,
}

#[derive(Debug)]
pub struct Parser {
	pub lexer: Tokeniser,
	pub nodes: Vec<ASTFunction>,
	pub custom_types: Vec<ASTClass>,
}

impl Parser {
	pub fn new(lexer: Tokeniser) -> Self {
		Self {
			lexer,
			nodes: Vec::new(),
			custom_types: Vec::new(),
		}
	}

	pub fn custom_type(&self, name: &str) -> Option<ASTClass> {
		// TODO: add lifetiems so we can use a ref
		self.custom_types.iter().find(|ct| ct.name == name).cloned()
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
				TokenType::Ident => {
					// TODO: proper ident parsing
					if self.skip_white_peek().unwrap().type_() == TokenType::Dot {
						let ident = arg.value().into();
						// skip the dot
						self.skip_white().unwrap();
						let dotted = self.skip_white().unwrap();
						if dotted.type_() != TokenType::Ident {
							panic!("Expected ident after dot");
						}
						args.push(ASTFunctionCallArg::DottedIdent((
							ident,
							dotted.value().into(),
						)))
					} else {
						args.push(ASTFunctionCallArg::Ident(arg.value().into()))
					}
				}
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

	fn parse_value_token(
		&mut self,
		ctx: &BlockCtx,
		target_type: &ASTType,
		value: &Token,
	) -> ASTAssignArg {
		match value.type_() {
			TokenType::IntLit => {
				if let ASTType::Int32(_) = target_type {
					// I've forgotten how to do this properly
				} else {
					panic!("Expected int Token")
				}

				let value = StaticValue::Int32(value.value().parse::<i32>().unwrap());
				ASTAssignArg::Static(ASTStaticAssign {
					value,
					value_type: ASTType::Int32(ASTInt32Type {}),
				})
			}
			TokenType::StringLit => {
				if let ASTType::String(_) = target_type {
					// I've forgotten how to do this properly
				} else {
					panic!("Expected String")
				}

				let value = StaticValue::String(value.value().into());
				ASTAssignArg::Static(ASTStaticAssign {
					value,
					value_type: ASTType::String(ASTStringType::default()),
				})
			}
			TokenType::Ident => {
				if let Some(var) = ctx.variables.iter().find(|v| v.ident == value.value()) {
					if !var.ast_type.has_same_type(target_type) {
						panic!("Different types: {target_type:#?} {:#?}", var.ast_type)
					}
				} else if self.skip_white_peek().unwrap().type_() != TokenType::LeftParen {
					// Panic for everything else execpt functions
					panic!("Variable {} not found! {:#?}", value.value(), ctx)
				};
				// TODO: properly create the type info
				ASTAssignArg::Ident(ASTAssignIdent {
					ident: value.value().into(),
					ident_type: target_type.clone(),
				})
			}
			_ => unimplemented!("{value:#?}"),
		}
	}

	fn parse_class_init(&mut self, ctx: &BlockCtx, target_type: &ASTType) -> ASTClassInit {
		// LeftBrace is skipped in parse_assign_expr so we can ignore it
		let class = if let ASTType::Custom(class) = target_type {
			class
		} else {
			panic!("Expected a class type {target_type:#?}")
		};

		let mut inits = Vec::new();
		let mut token = self.skip_white().unwrap();
		while token.type_() != TokenType::RightBrace {
			let ident: String = token.value().into();
			let member = if let Some(member) = class.member(&ident) {
				member
			} else {
				panic!("Member '{ident}' is not part of the class {}", class.name)
			};

			token = self.skip_white().unwrap();
			if token.type_() != TokenType::Colon {
				panic!("Expected colon");
			}
			token = self.skip_white().unwrap();

			let arg = self.parse_value_token(ctx, &member.type_, &token);
			inits.push(ASTClassInitArg { arg, ident });

			token = self.skip_white().unwrap();
			if token.type_() == TokenType::Comma {
				token = self.skip_white().unwrap();
			}
		}

		ASTClassInit {
			args: inits,
			name: class.name.clone(),
		}
	}

	fn parse_assign_expr(&mut self, ctx: &BlockCtx, target_type: &ASTType) -> ASTAssignmentExpr {
		let value_token = self.skip_white().unwrap();
		if value_token.type_() == TokenType::LeftBrace {
			// TODO: make sure members have the right type
			let class = self.parse_class_init(ctx, target_type);
			return ASTAssignmentExpr::ClassInit(class);
		}
		let value = self.parse_value_token(ctx, target_type, &value_token);
		let peek = self.skip_white_peek().unwrap();
		let expr = match peek.type_() {
			TokenType::AddOp => {
				// Skip the OpToken
				let token = self.skip_white().unwrap();
				// get the assignment
				let rhs = self.skip_white().unwrap();
				let rhs = self.parse_value_token(ctx, target_type, &rhs);
				if token.value() == "+" {
					ASTAssignmentExpr::Add(ASTAdd { lhs: value, rhs })
				} else {
					ASTAssignmentExpr::Minus(ASTMinus { lhs: value, rhs })
				}
			}
			TokenType::LeftParen => {
				// Skip the LeftParen
				self.skip_white().unwrap();
				let fn_call = self.parse_function_call(value_token.value(), true);
				ASTAssignmentExpr::FunctionCall(fn_call)
			}
			_ => ASTAssignmentExpr::Arg(value),
		};

		expr
	}

	fn parse_ast_type(&self, type_token: &Token) -> ASTType {
		if type_token.value() == "String" {
			ASTType::String(ASTStringType::default())
		} else if type_token.value() == "Int32" {
			ASTType::Int32(ASTInt32Type {})
		} else if let Some(custom) = self.custom_type(type_token.value()) {
			ASTType::Custom(custom)
		} else {
			panic!("Type '{}' not found", type_token.value())
		}
	}

	fn parse_assignment(
		&mut self,
		ctx: &BlockCtx,
		type_token: Token,
		ident: Token,
	) -> ASTAssignment {
		let ast_type = self.parse_ast_type(&type_token);

		let assign = self.skip_white().unwrap();
		if assign.type_() != TokenType::AssignOp {
			panic!("Expected assign Op {assign:#?}");
		}

		let expr = self.parse_assign_expr(ctx, &ast_type);
		let variable = ASTVariable {
			ast_type,
			ident: ident.value().into(),
		};

		ASTAssignment { expr, variable }
	}

	fn parse_return_statement(&mut self, ctx: &BlockCtx) -> ASTReturn {
		// TODO: Target type should be block's type
		let expr = self.parse_assign_expr(ctx, &ASTType::Int32(ASTInt32Type {}));
		ASTReturn { expr }
	}

	fn parse_if_stmt(&mut self, ctx: &BlockCtx) -> ASTIfStmt {
		let value_token = self.skip_white().unwrap();
		// TODO: poperly figure out the target type
		let target_type = ASTType::Int32(ASTInt32Type {});
		let value = self.parse_value_token(ctx, &target_type, &value_token);

		let peek = self.skip_white_peek().unwrap();
		let conditional = match peek.type_() {
			TokenType::RelOp if peek.value() == "<" => {
				// Skip the OpToken
				self.skip_white().unwrap();
				// get the assignment
				let rhs = self.skip_white().unwrap();
				let rhs = self.parse_value_token(ctx, &target_type, &rhs);
				ASTLtStmt { lhs: value, rhs }
			}
			_ => panic!("I have no idea what's happening"),
		};

		let block = self.parse_block(None);
		ASTIfStmt { block, conditional }
	}

	fn parse_block(&mut self, variables: Option<Vec<ASTVariable>>) -> ASTBlock {
		let left_brace = self.skip_white().unwrap();
		if left_brace.type_() != TokenType::LeftBrace {
			panic!("Expected left brace");
		}

		let mut ctx = BlockCtx {
			variables: if let Some(v) = variables {
				v
			} else {
				Vec::new()
			},
		};
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
					let assign = self.parse_assignment(&ctx, statement, next);
					ctx.variables.push(assign.variable.clone());
					statements.push(ASTBlockStatement::Assignment(assign))
				} else {
					panic!("Expected left paren or identifier {next:#?}")
				}
			} else if statement.type_() == TokenType::ReturnStmt {
				let return_stmt = self.parse_return_statement(&ctx);
				statements.push(ASTBlockStatement::Return(return_stmt))
			} else if statement.type_() == TokenType::IfStmt {
				let if_stmt = self.parse_if_stmt(&ctx);
				statements.push(ASTBlockStatement::IfStmt(if_stmt))
			} else {
				panic!("Expected identifier got: {statement:#?}")
			};

			statement = self.skip_white().unwrap();
		}

		ASTBlock::new(statements, ctx.variables)
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

		// TODO: args should be the same as variables
		let mut args: Vec<ASTVariable> = Vec::new();
		let mut variables: Vec<ASTVariable> = Vec::new();
		let mut ident = self.skip_white().unwrap();
		while ident.type_() != TokenType::RightParen {
			// We don't need the type right now so just ignore it
			if ident.type_() != TokenType::Ident {
				panic!("Expected identifier {ident:#?}");
			}

			let ast_type = self.parse_ast_type(&ident);

			ident = self.skip_white().unwrap();
			if ident.type_() != TokenType::Ident {
				panic!("Expected identfier");
			}

			args.push(ASTVariable {
				ast_type: ast_type.clone(),
				ident: ident.value().into(),
			});
			variables.push(ASTVariable {
				ast_type: ASTType::Int32(ASTInt32Type {}),
				ident: ident.value().into(),
			});

			// Parse, but ignore commas
			ident = self.skip_white().unwrap();
			if ident.type_() == TokenType::Comma {
				ident = self.skip_white().unwrap();
			}
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

		let body = self.parse_block(Some(variables));

		ASTFunction::new(name.into(), args, body, returns)
	}

	fn parse_class(&mut self) -> ASTClass {
		let ident = self.skip_white().unwrap();
		let name = if ident.type_() == TokenType::Ident {
			ident.value().into()
		} else {
			panic!("Expected identifier after 'class'")
		};

		let left_brace = self.skip_white().unwrap();
		if left_brace.type_() != TokenType::LeftBrace {
			panic!("Expected left brace");
		}

		let mut members: Vec<ASTClassMember> = Vec::new();
		let mut ident = self.skip_white().unwrap();
		while ident.type_() != TokenType::RightBrace {
			if ident.type_() != TokenType::Ident {
				panic!("Expected ident in funciton");
			}

			// TODO: self reference in types
			let type_ = self.parse_ast_type(&ident);

			ident = self.skip_white().unwrap();
			if ident.type_() != TokenType::Ident {
				panic!("Expected identfier {ident:#?}");
			}

			members.push(ASTClassMember {
				type_,
				ident: ident.value().into(),
			});

			ident = self.skip_white().unwrap();
		}

		ASTClass { name, members }
	}

	pub fn parse(&mut self) {
		let mut current = self.skip_white();

		while let Ok(token) = current {
			if token.type_() == TokenType::Eof {
				break;
			} else if token.type_() == TokenType::FunctionDef {
				let function = self.parse_function();
				self.nodes.push(function);
			} else if token.type_() == TokenType::ClassDef {
				let class = self.parse_class();
				self.custom_types.push(class.clone());
			} else {
				unimplemented!("Only FunctionDef statements can be parsed! {token:?}")
			}

			current = self.skip_white();
		}
	}
}
