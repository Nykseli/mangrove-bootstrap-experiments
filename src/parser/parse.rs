use crate::ast::{
	ASTAdd, ASTArrayAccess, ASTArrayInit, ASTArrayType, ASTAssignArg, ASTAssignDottedIdent,
	ASTAssignIdent, ASTAssignment, ASTAssignmentExpr, ASTBlock, ASTBlockStatement, ASTClass,
	ASTClassInit, ASTClassInitArg, ASTClassMember, ASTEnum, ASTEnumValue, ASTFunction,
	ASTFunctionCall, ASTFunctionCallArg, ASTIdent, ASTIfStmt, ASTInt32Type, ASTLtStmt, ASTMinus,
	ASTPointerType, ASTReturn, ASTStaticAssign, ASTStringType, ASTType, ASTVariable, StaticValue,
};

use super::tokeniser::Tokeniser;
use super::types::{Token, TokenType};

#[derive(Debug)]
struct BlockCtx {
	variables: Vec<ASTVariable>,
}

impl BlockCtx {
	fn find_variable<'a>(&'a self, name: &str) -> Option<&'a ASTVariable> {
		self.variables.iter().find(|v| v.ident == name)
	}
}

#[derive(Debug)]
pub struct Parser {
	pub lexer: Tokeniser,
	pub nodes: Vec<ASTFunction>,
	pub class_types: Vec<ASTClass>,
	pub enum_types: Vec<ASTEnum>,
}

impl Parser {
	pub fn new(lexer: Tokeniser) -> Self {
		Self {
			lexer,
			nodes: Vec::new(),
			class_types: Vec::new(),
			enum_types: Vec::new(),
		}
	}

	pub fn class_type(&self, name: &str) -> Option<ASTClass> {
		// TODO: add lifetiems so we can use a ref
		self.class_types.iter().find(|ct| ct.name == name).cloned()
	}

	pub fn enum_type(&self, name: &str) -> Option<ASTEnum> {
		// TODO: add lifetiems so we can use a ref
		self.enum_types.iter().find(|ct| ct.name == name).cloned()
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

	fn parse_function_call(
		&mut self,
		variable: Option<ASTVariable>,
		name: &str,
		used: bool,
	) -> ASTFunctionCall {
		// left paren is handled by parse_block for now
		/* let left_paren = self.skip_white().unwrap();
		if left_paren.type_() != TokenType::LeftParen {
			panic!("Expected left paren {left_paren:#?}");
		} */

		let is_static = if let Some(var) = &variable {
			if let ASTType::Class(class) = &var.ast_type {
				class
					.method(name)
					.unwrap_or_else(|| {
						panic!("Class {} doesn't have the method {name}", class.name)
					})
					.static_
			} else {
				false
			}
		} else {
			false
		};

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

		ASTFunctionCall::new(name.into(), variable, args, used, is_static)
	}

	fn parse_value_token(
		&mut self,
		ctx: &BlockCtx,
		target_type: &ASTType,
		value: &Token,
	) -> ASTAssignArg {
		let value = if value.type_() == TokenType::AddOp {
			let next = self.skip_white_peek().unwrap();
			if next.type_() != TokenType::IntLit {
				panic!("Expected IntLit after AddOP, found {value:#?}");
			}
			let mut int = self.skip_white().unwrap();
			int.set_value(format!("{}{}", value.value(), int.value()));
			int
		} else {
			value.clone()
		};

		match value.type_() {
			TokenType::IntLit => match target_type {
				ASTType::Int32(_) => {
					let value = StaticValue::Int32(value.value().parse::<i32>().unwrap());
					ASTAssignArg::Static(ASTStaticAssign {
						value,
						value_type: ASTType::Int32(ASTInt32Type {}),
					})
				}
				ASTType::Int64 => {
					let value = StaticValue::Int64(value.value().parse::<i64>().unwrap());
					ASTAssignArg::Static(ASTStaticAssign {
						value,
						value_type: ASTType::Int64,
					})
				}
				_ => panic!("Expected int Token"),
			},
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
				let peek_next = self.skip_white_peek().unwrap();
				if peek_next.type_() == TokenType::Dot {
					let ident: String = value.value().into();
					// skip the dot
					self.skip_white().unwrap();
					let dotted = self.skip_white().unwrap();
					if dotted.type_() != TokenType::Ident {
						panic!("Expected ident after dot");
					}

					if let Some(var) = ctx.variables.iter().find(|v| v.ident == value.value()) {
						// TODO: Buildin type member checking
						if let ASTType::Class(class) = &var.ast_type {
							let member = class
								.member(dotted.value())
								.unwrap_or_else(|| panic!("{} member not found", dotted.value()));
							if !member.type_.has_same_type(target_type) {
								panic!("Different types: {target_type:#?} {:#?}", member.type_)
							}
						} else {
							panic!("Only custom type dotted args implemented");
						}
					} else if let Some(enum_) = self.enum_type(value.value()) {
						if enum_.value(dotted.value()).is_none() {
							panic!(
								"Enum '{}' doesn\'t have a value '{}'",
								value.value(),
								dotted.value()
							);
						}
					} else {
						// Panic for everything else execpt functions
						panic!("Variable {} not found!", value.value());
					};

					return ASTAssignArg::DottedIdent(ASTAssignDottedIdent {
						ident: (ident, dotted.value().into()),
						ident_type: target_type.clone(),
					});
				}

				if let Some(var) = ctx.variables.iter().find(|v| v.ident == value.value()) {
					if var.ast_type.has_same_type(target_type) {
						// noop
					} else if let ASTType::Array(array) = &var.ast_type {
						if !array.type_.has_same_type(target_type) {
							panic!("Different types: {target_type:#?} {:#?}", array.type_)
						}
					} else if !var.ast_type.has_same_type(target_type) {
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
		let class = if let ASTType::Class(class) = target_type {
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

			let arg = self.parse_assign_expr(ctx, &member.type_);
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

	fn parse_array_init(&mut self, ctx: &BlockCtx, target_type: &ASTType) -> ASTArrayInit {
		let array = if let ASTType::Array(array) = target_type {
			array
		} else {
			panic!("Expected a array type {target_type:#?}")
		};

		let mut inits = Vec::new();
		/* 		let mut token = self.skip_white().unwrap(); */
		let mut token = Token::default();
		while token.type_() != TokenType::RightSquare {
			let arg = self.parse_assign_expr(ctx, &array.type_);
			inits.push(arg);

			token = self.skip_white_peek().unwrap();
			if token.type_() == TokenType::Comma {
				token = self.skip_white().unwrap();
			}
		}

		// array.size is -1 if the size is defined by the expression
		if array.size > 0 && inits.len() != array.size as usize {
			panic!(
				"Array init length was {}, expected: {}",
				inits.len(),
				array.size
			);
		}

		self.skip_white().unwrap();
		ASTArrayInit { args: inits }
	}

	fn parse_assign_expr(&mut self, ctx: &BlockCtx, target_type: &ASTType) -> ASTAssignmentExpr {
		let value_token = self.skip_white().unwrap();
		if value_token.type_() == TokenType::LeftBrace {
			// TODO: make sure members have the right type
			let class = self.parse_class_init(ctx, target_type);
			return ASTAssignmentExpr::ClassInit(class);
		}
		if value_token.type_() == TokenType::LeftSquare {
			let array = self.parse_array_init(ctx, target_type);
			return ASTAssignmentExpr::ArrayInit(array);
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
				let fn_call = self.parse_function_call(None, value_token.value(), true);
				ASTAssignmentExpr::FunctionCall(fn_call)
			}
			TokenType::LeftSquare => {
				// skip the left square
				self.skip_white().unwrap();
				let ident = match value {
					ASTAssignArg::Static(_) => panic!("Expected ident"),
					ASTAssignArg::Ident(ident) => ASTIdent::Ident(ident.ident),
					ASTAssignArg::DottedIdent(ident) => {
						ASTIdent::DottedIdent((ident.ident.0.clone(), ident.ident.1))
					}
				};

				// We can only access with ints so the value should resolve into it
				let arg = self.parse_assign_expr(ctx, &ASTType::Int32(ASTInt32Type {}));
				let right = self.skip_white().unwrap();
				if right.type_() != TokenType::RightSquare {
					panic!("Expected RightSquare");
				}

				ASTAssignmentExpr::ASTArrayAccess(ASTArrayAccess {
					ident,
					arg: Box::new(arg),
				})
			}
			_ => ASTAssignmentExpr::Arg(value),
		};

		expr
	}

	fn parse_array_type(&mut self) -> ASTArrayType {
		let mut ident = self.skip_white().unwrap();
		// Nested Arrays
		if ident.type_() == TokenType::RelOp && ident.value() == "<" {
			ident = self.skip_white().unwrap();
		}

		if ident.type_() != TokenType::Ident {
			panic!("Expected identifier {ident:#?}");
		}

		let ast_type = self.parse_ast_type(&ident, None);

		let mut next = self.skip_white().unwrap();
		let int_lit: i32 = if next.type_() == TokenType::Comma {
			let int_lit = self.skip_white().unwrap();
			if int_lit.type_() != TokenType::IntLit {
				panic!("Expected int literal {int_lit:#?}");
			}

			next = self.skip_white().unwrap();
			int_lit.value().parse::<i32>().unwrap()
		} else {
			-1
		};

		if next.type_() != TokenType::RelOp || next.value() != ">" {
			panic!("Expected '>' {next:#?}");
		}

		ASTArrayType {
			type_: Box::new(ast_type),
			size: int_lit,
		}
	}

	fn parse_ast_type(&mut self, type_token: &Token, next: Option<&Token>) -> ASTType {
		let type_ = if type_token.value() == "String" {
			ASTType::String(ASTStringType::default())
		} else if type_token.value() == "Int64" {
			ASTType::Int64
		} else if type_token.value() == "Int32" {
			ASTType::Int32(ASTInt32Type {})
		} else if type_token.value() == "Array" {
			ASTType::Array(self.parse_array_type())
		} else if let Some(custom) = self.class_type(type_token.value()) {
			ASTType::Class(custom)
		} else if let Some(custom) = self.enum_type(type_token.value()) {
			ASTType::Enum(custom)
		} else {
			panic!("Type '{}' not found", type_token.value())
		};

		let pre_peeked = self.skip_white_peek().unwrap();
		let peek = if let Some(peek) = next {
			peek
		} else {
			&pre_peeked
		};

		if peek.type_() == TokenType::MulOp && peek.value() == "*" {
			ASTType::Pointer(ASTPointerType {
				type_: Box::new(type_),
			})
		} else {
			type_
		}
	}

	fn parse_assignment(
		&mut self,
		ctx: &BlockCtx,
		type_token: Token,
		next: Token,
	) -> ASTAssignment {
		let mut ast_type = self.parse_ast_type(&type_token, Some(&next));

		let ident = if next.type_() == TokenType::RelOp || next.type_() == TokenType::MulOp {
			// Assignment type was templated, or a ptr type so the next value is the ident
			ASTIdent::Ident(self.skip_white().unwrap().value().into())
		} else {
			ASTIdent::Ident(next.value().into())
		};

		let assign = self.skip_white().unwrap();
		if assign.type_() != TokenType::AssignOp {
			panic!("Expected assign Op {assign:#?}");
		}

		let expr = self.parse_assign_expr(ctx, &ast_type);
		// Reassign the length of the array type in case it's defined by the expression
		if let ASTAssignmentExpr::ArrayInit(ainit) = &expr {
			if let ASTType::Array(arr) = &mut ast_type {
				arr.size = ainit.args.len() as i32
			}
		}
		let variable = ASTVariable { ast_type, ident };

		ASTAssignment {
			expr,
			variable,
			reassignment: false,
		}
	}

	fn parse_reassignment(
		&mut self,
		ctx: &BlockCtx,
		ast_type: ASTType,
		ident: ASTIdent,
	) -> ASTAssignment {
		let expr = self.parse_assign_expr(ctx, &ast_type);
		let variable = ASTVariable { ast_type, ident };

		ASTAssignment {
			expr,
			variable,
			reassignment: true,
		}
	}

	fn parse_return_statement(
		&mut self,
		ctx: &BlockCtx,
		return_type: Option<&ASTType>,
	) -> ASTReturn {
		// TODO: support none return types
		let ret = return_type.unwrap_or_else(|| panic!("Return type cannot be none"));
		// TODO: Target type should be block's type
		let expr = self.parse_assign_expr(ctx, ret);
		ASTReturn { expr }
	}

	fn parse_if_stmt(&mut self, ctx: &BlockCtx, return_type: Option<&ASTType>) -> ASTIfStmt {
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

		let block = self.parse_block(None, return_type);
		ASTIfStmt { block, conditional }
	}

	fn parse_ident_statement(&mut self, ctx: &mut BlockCtx, statement: Token) -> ASTBlockStatement {
		let next = self.skip_white().unwrap();
		if next.type_() == TokenType::LeftParen {
			let fn_call = self.parse_function_call(None, statement.value(), false);
			ASTBlockStatement::FunctionCall(fn_call)
		} else if next.type_() == TokenType::Ident
			|| next.type_() == TokenType::RelOp
			|| (next.type_() == TokenType::MulOp && next.value() == "*")
		{
			// Ident after type
			let assign = self.parse_assignment(ctx, statement, next);
			ctx.variables.push(assign.variable.clone());
			ASTBlockStatement::Assignment(assign)
		} else if next.type_() == TokenType::AssignOp {
			// Assign into existing variable
			let var = ctx
				.find_variable(statement.value())
				.unwrap_or_else(|| panic!("Variable '{}' not defined", statement.value()));
			let assign =
				self.parse_reassignment(ctx, var.ast_type.clone(), statement.value().into());
			ASTBlockStatement::Assignment(assign)
		} else if next.type_() == TokenType::Dot {
			// Parsing dotted indet/expression
			let dotted = self.skip_white().unwrap();
			if dotted.type_() != TokenType::Ident {
				panic!("Expected ident: {dotted:#?}");
			}

			let next_peek = self.skip_white_peek().unwrap();

			// Ident function call
			if next_peek.type_() == TokenType::LeftParen {
				self.skip_white().unwrap();
				let fn_call = if let Some(var) = ctx.find_variable(statement.value()) {
					// regular method call
					let fn_call =
						self.parse_function_call(Some(var.clone()), dotted.value(), false);
					if fn_call.static_ {
						panic!("Class method '{}' is static", dotted.value())
					}
					fn_call
				} else {
					// Static method call
					let class = self
						.class_type(statement.value())
						.unwrap_or_else(|| panic!("Class '{}' is not defined", statement.value()));
					// hacky temp variable
					let tmpvar = ASTVariable {
						ast_type: ASTType::Class(class.clone()),
						ident: "".into(),
					};
					let fn_call = self.parse_function_call(Some(tmpvar), dotted.value(), false);
					if !fn_call.static_ {
						panic!(
							"Class method '{}' in '{}' is not static",
							&class.name,
							dotted.value()
						)
					}
					fn_call
				};
				ASTBlockStatement::FunctionCall(fn_call)
			} else {
				let var = ctx
					.find_variable(statement.value())
					.unwrap_or_else(|| panic!("Variable '{}' not defined", statement.value()));
				// reassign into dotted ident
				self.skip_white().unwrap();
				let dotted_type = match &var.ast_type {
					ASTType::Int64 => todo!(),
					ASTType::Int32(_) => todo!(),
					ASTType::String(_) => todo!(),
					ASTType::Class(class) => class
						.member(dotted.value())
						.unwrap_or_else(|| {
							panic!(
								"Class {} doesn't have a member {}",
								class.name,
								dotted.value()
							)
						})
						.type_
						.clone(),
					ASTType::Array(_) => todo!(),
					ASTType::Enum(_) => todo!(),
					ASTType::Pointer(_) => todo!(),
				};

				let dotted =
					ASTIdent::DottedIdent((statement.value().into(), dotted.value().into()));
				let assign = self.parse_reassignment(ctx, dotted_type, dotted);
				ASTBlockStatement::Assignment(assign)
			}
		} else {
			panic!("Expected left paren or identifier {next:#?}")
		}
	}

	fn parse_block(
		&mut self,
		variables: Option<Vec<ASTVariable>>,
		return_type: Option<&ASTType>,
	) -> ASTBlock {
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
				statements.push(self.parse_ident_statement(&mut ctx, statement))
			} else if statement.type_() == TokenType::ReturnStmt {
				let return_stmt = self.parse_return_statement(&ctx, return_type);
				statements.push(ASTBlockStatement::Return(return_stmt))
			} else if statement.type_() == TokenType::IfStmt {
				let if_stmt = self.parse_if_stmt(&ctx, return_type);
				statements.push(ASTBlockStatement::IfStmt(if_stmt))
			} else {
				panic!("Expected identifier got: {statement:#?}")
			};

			statement = self.skip_white().unwrap();
		}

		ASTBlock::new(statements, ctx.variables)
	}

	/// This refers to the current class/object
	fn parse_function(&mut self, this: Option<ASTVariable>) -> ASTFunction {
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
		let mut args: Vec<ASTVariable> = if let Some(this) = this {
			vec![this]
		} else {
			Vec::new()
		};

		let mut variables: Vec<ASTVariable> = args.clone();
		let mut ident = self.skip_white().unwrap();
		while ident.type_() != TokenType::RightParen {
			// We don't need the type right now so just ignore it
			if ident.type_() != TokenType::Ident {
				panic!("Expected identifier {ident:#?}");
			}

			let ast_type = self.parse_ast_type(&ident, None);

			ident = self.skip_white().unwrap();
			if ident.type_() != TokenType::Ident {
				panic!("Expected identfier");
			}

			args.push(ASTVariable {
				ast_type: ast_type.clone(),
				ident: ident.value().into(),
			});
			variables.push(ASTVariable {
				ast_type,
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
			TokenType::NoneType => None,
			// Just assuming that it's Int32
			TokenType::Ident => Some(self.parse_ast_type(&return_type, None)),
			_ => panic!("Didn't expect {return_type:#?}"),
		};

		let body = self.parse_block(Some(variables), returns.as_ref());

		ASTFunction::new(name.into(), args, body, returns)
	}

	fn parse_class(&mut self) -> ASTClass {
		let ident = self.skip_white().unwrap();
		let name: String = if ident.type_() == TokenType::Ident {
			ident.value().into()
		} else {
			panic!("Expected identifier after 'class'")
		};

		let left_brace = self.skip_white().unwrap();
		if left_brace.type_() != TokenType::LeftBrace {
			panic!("Expected left brace");
		}

		let mut members: Vec<ASTClassMember> = Vec::new();
		let mut methods: Vec<ASTFunction> = Vec::new();
		let mut ident = self.skip_white().unwrap();
		while ident.type_() != TokenType::RightBrace {
			let is_static = if ident.type_() == TokenType::StorageSpec && ident.value() == "static"
			{
				ident = self.skip_white().unwrap();
				true
			} else {
				false
			};

			if ident.type_() == TokenType::FunctionDef {
				// TODO: this creates a lot copies, optimise it
				let method = if is_static {
					let mut method = self.parse_function(None);
					method.static_ = true;
					method
				} else {
					let class = ASTType::Class(ASTClass {
						members: members.clone(),
						name: name.clone(),
						methods: Vec::new(),
					});

					let var = ASTVariable {
						ident: "this".into(),
						ast_type: class,
					};

					self.parse_function(Some(var))
				};

				methods.push(method);
				ident = self.skip_white().unwrap();
				continue;
			}

			if ident.type_() != TokenType::Ident {
				panic!("Expected ident in funciton {ident:#?}");
			}

			// TODO: self reference in types
			let type_ = self.parse_ast_type(&ident, None);

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

		ASTClass {
			name,
			members,
			methods,
		}
	}

	fn parse_enum_def(&mut self) -> ASTEnum {
		let ident = self.skip_white().unwrap();
		let name: String = if ident.type_() == TokenType::Ident {
			ident.value().into()
		} else {
			panic!("Expected identifier after 'enum'")
		};

		let left_brace = self.skip_white().unwrap();
		if left_brace.type_() != TokenType::LeftBrace {
			panic!("Expected left brace");
		}

		let mut values: Vec<ASTEnumValue> = Vec::new();
		let mut value: i32 = 0;
		let mut ident = self.skip_white().unwrap();
		while ident.type_() != TokenType::RightBrace {
			if ident.type_() != TokenType::Ident {
				panic!("Expected ident in enum {ident:#?}");
			}

			if values.iter().any(|v| v.name == ident.value()) {
				panic!("Value '{}' redifined in enum '{name}'", ident.value());
			}

			values.push(ASTEnumValue {
				name: ident.value().into(),
				value,
			});

			value += 1;
			ident = self.skip_white().unwrap();
		}

		ASTEnum { name, values }
	}

	pub fn parse(&mut self) {
		let mut current = self.skip_white();

		while let Ok(token) = current {
			if token.type_() == TokenType::Eof {
				break;
			} else if token.type_() == TokenType::FunctionDef {
				let function = self.parse_function(None);
				self.nodes.push(function);
			} else if token.type_() == TokenType::ClassDef {
				let class = self.parse_class();
				self.class_types.push(class.clone());
			} else if token.type_() == TokenType::EnumDef {
				let enum_ = self.parse_enum_def();
				self.enum_types.push(enum_.clone());
			} else if token.type_() == TokenType::Comment {
				// Just ignore for now
			} else {
				unimplemented!("Only FunctionDef statements can be parsed! {token:?}")
			}

			current = self.skip_white();
		}
	}
}
