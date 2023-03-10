use inkwell::types::BasicMetadataTypeEnum;
use inkwell::values::{AnyValue, AnyValueEnum};
use crate::compiler::{BuiltValueResult, Compiler};
use crate::compiler::error::IRBuildError;
use crate::compiler::error::IRBuildError::*;
use crate::parser::ast;
use crate::parser::ast::Expression;
use crate::parser::ast::Expression::*;

type CompileResult<'a> = Result<AnyValueEnum<'a>, IRBuildError>;

impl<'ctx> Compiler<'ctx> {
    pub fn compile(&self, ast: &Vec<ast::ASTNode>) -> CompileResult {
        let mut result = Err(EmptyASTError);

        for node in ast.iter() {
            result = Ok(self.compile_node(node)?)
        }
        result
    }

    fn compile_node(&self, node: &ast::ASTNode) -> CompileResult {
        match node {
            ast::ASTNode::EffectNode(ref effect) => {
                self.compile_expr(effect)
            },
        }
    }

    fn compile_expr(&self, expr: &Expression) -> CompileResult {
        match expr {
            IntegerLiteralExpr(val) => Ok(
                self.context.i32_type().const_int(*val as u64, true).into()
            ),
            FloatLiteralExpr(val) => Ok(
                self.context.f64_type().const_float(*val).into()
            ),
            StringLiteralExpr(val) => Ok(
                self.context.const_string(val.as_ref(), false).into()
            ),

            EvalExpr(name) => match self.get_value(name) {
                BuiltValueResult::Val(var) => {
                    Ok(self.builder.build_load(var, name).into())
                },
                BuiltValueResult::Eff(effect) => {
                    Ok(effect.into())
                },
                BuiltValueResult::None(_) => {
                    Err(VariableNotFoundError { name: name.to_string() })
                }
            },

            CallExpr(name, args) => {
                let function = match self.get_value(name) {
                    BuiltValueResult::Eff(effect) => effect,

                    BuiltValueResult::Val(_) => {
                        return Err(NotCallableError)
                    },
                    BuiltValueResult::None(_) => {
                        return Err(FunctionNotFoundError { name: name.to_string() })
                    }
                };

                let required = function.count_params() as usize;
                let provided = args.len();

                if required != provided {
                    return Err(IncorrectArgumentListError { required, provided })
                }

                let mut arg_vals = Vec::new();
                for arg in args.iter() {
                    let val = self.compile_expr(arg)?;
                    arg_vals.push(val.into_float_value().into());
                }

                match self.builder.build_call(function, arg_vals.as_slice(), "tmp").try_as_basic_value().left() {
                    Some(value) => Ok(value.as_any_value_enum()),
                    None => return Err(InvalidFunctionCallError)
                }
            },

            AssignExpr(vars, expr) => {
                let rhs = self.compile_expr(expr)?;
                for var in vars {

                    let eval = match var {
                        EvalExpr(_) => self.compile_expr(var)?,
                        _ => return Err(InvalidAssignmentOperationError)
                    };

                    let name = eval.to_string();
                    let var = self.values.get(name.as_str());

                    //self.builder.build_store(*var, rhs.into_float_value());
                    // todo
                }
                Ok(rhs)
            },

            BlockExpr(args, block) => {
                let ret_type = self.context.f64_type();

                let param_types = std::iter::repeat(ret_type)
                    .take(args.len())
                    .map(|i| i.into())
                    .collect::<Vec<BasicMetadataTypeEnum>>();

                let effect = self.new_effect(param_types, None);

                for (param, arg) in effect.get_params().iter().zip(args) {
                    // set correct parameters names
                    let name = match arg {
                        EvalExpr(name) => name,
                        _ => todo!()
                    };
                    param.set_name(name);
                }

                // todo currently only the first
                match block.first() {
                    Some(body) => {
                        let result = self.compile_expr(body)?.into_float_value();
                        self.builder.build_return(Some(&result));
                    }
                    None => {
                        self.builder.build_return(None);
                    }
                };

                if effect.verify(true) {

                    Ok(effect.into())
                } else {
                    unsafe { effect.delete() }
                    Err(InvalidEffectError)
                }
            },

            _ => todo!()
        }
    }
}
