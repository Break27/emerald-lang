use std::ops::Deref;
use inkwell::module::Module;
use inkwell::values::{AsValueRef, BasicMetadataValueEnum, BasicValue, BasicValueEnum};
use llvm_sys::prelude::LLVMValueRef;
use crate::compiler::Compiler;
use crate::compiler::error::IRBuildError;
use crate::compiler::error::IRBuildError::*;
use crate::parser;
use crate::parser::ast;
use crate::parser::ast::Expression::*;

type IRBuildingResult<'a> = Result<BasicValueEnum<'a>, IRBuildError>;

pub trait IRBuilder {
    fn codegen<'a>(&'a self, compiler: &'a &mut Compiler) -> IRBuildingResult;
}

impl IRBuilder for parser::ParsingResult {
    fn codegen<'a>(&'a self, compiler: &'a &mut Compiler) -> IRBuildingResult {
        match self {
            Ok((ref ast, _)) => ast.codegen(compiler),
            Err((e, _)) => Err(ParserError { message: e.to_string() })
        }
    }
}

impl IRBuilder for Vec<ast::ASTNode> {
    fn codegen<'a>(&'a self, compiler: &'a &mut Compiler) -> IRBuildingResult {

        let mut result = Err(EmptyASTError);

        for node in self.iter() {
            result = Ok(node.codegen(compiler)?)
        }
        result
    }
}

impl IRBuilder for ast::ASTNode {
    fn codegen<'a>(&'a self, compiler: &'a &mut Compiler) -> IRBuildingResult {
        match self {
            ast::ASTNode::EffectNode(ref effect) => effect.codegen(compiler),
        }
    }
}

impl IRBuilder for ast::Expression {
    fn codegen<'a>(&'a self, cp: &'a &mut Compiler) -> IRBuildingResult {
        match self {
            IntegerLiteralExpr(val) => Ok(
                cp.context.i32_type().const_int(*val as u64, true).as_basic_value_enum()
            ),
            FloatLiteralExpr(val) => Ok(
                cp.context.f64_type().const_float(*val).as_basic_value_enum()
            ),
            StringLiteralExpr(val) => Ok(
                cp.context.const_string(val.as_ref(), false).as_basic_value_enum()
            ),

            EvalExpr(name) => match cp.variables.get(name) {
                Some(var) => {
                    Ok(cp.builder.build_load(*var, name).as_basic_value_enum())
                },
                None => Err(VariableNotFound { name: name.to_string() })
            },

            CallExpr(name, args) => {
                let function = match cp.module.get_function(name) {
                    Some(func) => func,
                    None => return Err(FunctionNotFound { name: name.to_string() })
                };

                let required = function.count_params() as usize;
                let provided = args.len();

                if required != provided {
                    return Err(IncorrectArgumentList { required, provided })
                }

                let mut arg_vals = Vec::new();
                for arg in args.iter() {
                    let val = arg.codegen(cp)?;
                    arg_vals.push(val.into());
                }

                match cp.builder.build_call(function, arg_vals.as_slice(), "tmp").try_as_basic_value().left() {
                    Some(value) => Ok(value),
                    None => Err(InvalidFunctionCall)
                }
            },

            AssignExpr(vars, expr) => {
                let rhs = expr.codegen(cp)?;
                for var in vars {

                    let eval = if let EvalExpr(_) = var {
                        var.codegen(cp)?
                    } else {
                        return Err(InvalidAssignmentOperationError)
                    };

                    let name = eval.to_string();
                    let var = cp.variables
                        .get(name.as_str())
                        .ok_or(VariableNotFound { name })?;

                    cp.builder.build_store(*var, rhs);
                }
                Ok(rhs)
            },

            BlockExpr(args, block) => {


                todo!()
            },

            _ => todo!()
        }
    }
}