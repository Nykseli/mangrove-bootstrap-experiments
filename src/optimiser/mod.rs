use crate::{
	ast::{
		ASTAssignArg, ASTAssignmentExpr, ASTBlock, ASTBlockStatement, ASTFunction, ASTFunctionCall,
		ASTFunctionCallArg, ASTIdent, ASTInt32Type, ASTStaticAssign, ASTStringType, ASTType,
		StaticValue,
	},
	parser::parse::Parser,
};

impl ASTAssignArg {
	fn constant(&self, ctx: &mut OptimiserCtx) -> Option<ConstantValue> {
		let val = match self {
			ASTAssignArg::Static(val) => match &val.value {
				StaticValue::Int32(val) => ConstantValue::Int32(*val),
				StaticValue::Int64(val) => ConstantValue::Int64(*val),
				StaticValue::String(val) => ConstantValue::String(val.clone()),
				StaticValue::Char(_) => return None,
			},
			ASTAssignArg::Ident(ident) => {
				if let Some(var) = ctx.variable(&ident.ident) {
					var.value.clone()
				} else {
					return None;
				}
			}
			ASTAssignArg::DottedIdent(_) => return None,
			ASTAssignArg::Deref(_) => return None,
		};
		Some(val)
	}
}

impl ASTFunctionCallArg {
	fn constant(&self, ctx: &mut OptimiserCtx) -> Option<ConstantValue> {
		let val = match self {
			ASTFunctionCallArg::Char(_) => return None,
			ASTFunctionCallArg::Int32(_) => return None,
			ASTFunctionCallArg::Int64(_) => return None,
			ASTFunctionCallArg::String(_) => return None,
			ASTFunctionCallArg::Ident(ident) => {
				if let Some(var) = ctx.variable(ident) {
					var.value.clone()
				} else {
					return None;
				}
			}
			ASTFunctionCallArg::DottedIdent(_) => return None,
		};
		Some(val)
	}
}

impl ASTAssignmentExpr {
	fn constant(&self, ctx: &mut OptimiserCtx) -> Option<ConstantValue> {
		let val = match self {
			ASTAssignmentExpr::Arg(arg) => return arg.constant(ctx),
			ASTAssignmentExpr::Add(add) => match (add.lhs.constant(ctx), add.rhs.constant(ctx)) {
				(Some(val), Some(other)) => val.combine(&other),
				_ => return None,
			},
			ASTAssignmentExpr::Minus(_) => return None,
			ASTAssignmentExpr::FunctionCall(_) => return None,
			ASTAssignmentExpr::ClassInit(_) => return None,
			ASTAssignmentExpr::ArrayInit(_) => return None,
			ASTAssignmentExpr::ASTArrayAccess(_) => return None,
		};
		Some(val)
	}

	fn optimise(&mut self, ctx: &mut OptimiserCtx) {
		match self {
			// mark noops with ()
			ASTAssignmentExpr::Arg(_) => (),
			// ASTAssignmentExpr::DerefArg(_) => (),
			ASTAssignmentExpr::Add(_) => (),
			ASTAssignmentExpr::Minus(_) => (),
			ASTAssignmentExpr::FunctionCall(fn_call) => {
				for arg in &mut fn_call.args {
					if let Some(constant) = arg.constant(ctx) {
						match constant {
							ConstantValue::String(str) => {
								*arg = ASTFunctionCallArg::String(str.clone())
							}
							ConstantValue::Int32(int) => *arg = ASTFunctionCallArg::Int32(int),
							ConstantValue::Int64(_) => todo!(),
						}
					}
				}
			}
			ASTAssignmentExpr::ClassInit(init) => {
				for init in &mut init.args {
					if let Some(constant) = init.arg.constant(ctx) {
						init.arg = constant.into()
					}
				}
			}
			ASTAssignmentExpr::ArrayInit(init) => {
				for arg in &mut init.args {
					if let Some(constant) = arg.constant(ctx) {
						*arg = constant.into()
					}
				}
			}
			ASTAssignmentExpr::ASTArrayAccess(access) => {
				if let Some(constant) = access.arg.constant(ctx) {
					*access.arg = constant.into()
				}
			}
		}
	}
}

impl From<ConstantValue> for ASTAssignmentExpr {
	fn from(value: ConstantValue) -> Self {
		match value {
			ConstantValue::String(str) => {
				ASTAssignmentExpr::Arg(ASTAssignArg::Static(ASTStaticAssign {
					value: StaticValue::String(str),
					value_type: ASTType::String(ASTStringType {
						..Default::default()
					}),
				}))
			}
			ConstantValue::Int32(int) => {
				ASTAssignmentExpr::Arg(ASTAssignArg::Static(ASTStaticAssign {
					value: StaticValue::Int32(int),
					value_type: ASTType::Int32(ASTInt32Type {}),
				}))
			}
			ConstantValue::Int64(int) => {
				ASTAssignmentExpr::Arg(ASTAssignArg::Static(ASTStaticAssign {
					value: StaticValue::Int64(int),
					value_type: ASTType::Int64,
				}))
			}
		}
	}
}

#[derive(Debug, Clone)]
enum ConstantValue {
	String(String),
	Int32(i32),
	Int64(i64),
}

impl ConstantValue {
	fn combine(&self, other: &Self) -> Self {
		match (&self, &other) {
			(ConstantValue::Int32(i1), ConstantValue::Int32(i2)) => ConstantValue::Int32(i1 + i2),
			(ConstantValue::Int64(i1), ConstantValue::Int64(i2)) => ConstantValue::Int64(i1 + i2),
			(ConstantValue::String(s1), ConstantValue::String(s2)) => {
				let mut s = s1.clone();
				s.push_str(s2);
				ConstantValue::String(s)
			}
			_ => panic!("Cannot combine unmatching types"),
		}
	}
}

#[derive(Debug, Clone)]
struct ConstantVariable {
	ident: String,
	value: ConstantValue,
	/// Is variable referenced in a context where it needs to be saved.
	/// This is reserved for future use since strings and ints can always
	/// be optimised out for the time being
	_referenced: bool,
}

pub struct Optimiser {
	pub parser: Parser,
}

pub struct OptimiserCtx {
	vars: Vec<ConstantVariable>,
}

impl OptimiserCtx {
	fn variable<'a>(&'a self, name: &str) -> Option<&'a ConstantVariable> {
		self.vars.iter().find(|v| v.ident == name)
	}

	fn add_or_update(&mut self, var: ConstantVariable) {
		if let Some(found) = self.vars.iter_mut().find(|v| v.ident == var.ident) {
			found.value = var.value;
		} else {
			self.vars.push(var);
		}
	}
}

fn optimise_function(ctx: &mut OptimiserCtx, function: &mut ASTFunction) -> ASTFunction {
	let mut statements: Vec<ASTBlockStatement> = Vec::new();
	for statment in &mut function.body.statements {
		match statment {
			ASTBlockStatement::Assignment(assign) => {
				// Always internally optimise the assignment
				assign.expr.optimise(ctx);

				// TODO: dotted ident
				let ident = if let ASTIdent::Ident(ident) = &assign.variable.ident {
					ident.clone()
				} else {
					statements.push(statment.clone());
					continue;
				};

				let value = if let Some(val) = assign.expr.constant(ctx) {
					val
				} else {
					statements.push(statment.clone());
					continue;
				};

				ctx.add_or_update(ConstantVariable {
					ident,
					value,
					_referenced: false,
				})
			}
			ASTBlockStatement::FunctionCall(call) => {
				let mut args: Vec<ASTFunctionCallArg> = Vec::new();
				for arg in &call.args {
					match &arg {
						ASTFunctionCallArg::Ident(ident) => {
							if let Some(val) = ctx.variable(ident) {
								match &val.value {
									ConstantValue::String(s) => {
										args.push(ASTFunctionCallArg::String(s.clone()))
									}
									ConstantValue::Int32(i) => {
										args.push(ASTFunctionCallArg::Int32(*i))
									}
									ConstantValue::Int64(i) => {
										args.push(ASTFunctionCallArg::Int64(*i))
									}
								}
							} else {
								args.push(arg.clone())
							};
						}
						_ => args.push(arg.clone()),
					}
				}

				statements.push(ASTBlockStatement::FunctionCall(ASTFunctionCall {
					args,
					name: call.name.clone(),
					variable: call.variable.clone(),
					return_used: call.return_used,
					static_: call.static_,
				}));
				continue;
			}
			ASTBlockStatement::Return(_) => {
				statements.push(statment.clone());
				continue;
			}
			ASTBlockStatement::IfStmt(_) => {
				statements.push(statment.clone());
				continue;
			}
			ASTBlockStatement::DerefAssignment(_) => {
				statements.push(statment.clone());
				continue;
			}
		}
	}

	ASTFunction {
		args: function.args.clone(),
		name: function.name.clone(),
		body: ASTBlock {
			statements,
			// TODO: optimise out variables
			variables: function.body.variables.clone(),
		},
		static_: function.static_,
		returns: function.returns.clone(),
	}
}

impl Optimiser {
	pub fn new(parser: Parser) -> Self {
		Self { parser }
	}

	pub fn optimise(&mut self) {
		let mut ctx = OptimiserCtx { vars: Vec::new() };

		for class in &mut self.parser.class_types {
			for method in &mut class.methods {
				*method = optimise_function(&mut ctx, method)
			}
		}

		let mut functions: Vec<ASTFunction> = Vec::new();
		for function in &mut self.parser.nodes {
			functions.push(optimise_function(&mut ctx, function));
		}

		self.parser.nodes = functions;
	}
}
