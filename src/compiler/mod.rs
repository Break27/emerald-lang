use std::collections::HashMap;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, FloatType, FunctionType};
use inkwell::values::{AnyValue, AnyValueEnum, FunctionValue, PointerValue};

pub mod codegen;
pub mod error;

pub struct Effect<'a> {
    name: String,
    members: HashMap<String, PointerValue<'a>>
}

pub enum BuiltValueEnum<'a> {
    Value(PointerValue<'a>),
    Effect(Effect<'a>)
}

pub enum BuiltValueResult<'a> {
    Val(PointerValue<'a>),
    Eff(FunctionValue<'a>),
    None(usize)
}

pub struct Compiler<'ctx> {
    pub context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,

    values: HashMap<String, BuiltValueEnum<'ctx>>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {

        Compiler {
            context: &context,
            builder: context.create_builder(),
            module: context.create_module(module_name),
            values: HashMap::new(),
        }
    }
    
    pub fn get_value(&self, name: &str) -> BuiltValueResult<'ctx> {
        match self.values.get(name) {
            Some(BuiltValueEnum::Value(var)) => {
                BuiltValueResult::Val(*var)
            },
            Some(BuiltValueEnum::Effect(effect)) => {
                match self.module.get_function(&*effect.name) {
                    Some(val) => BuiltValueResult::Eff(val),
                    None => BuiltValueResult::None(1)
                }
            },
            None => BuiltValueResult::None(0)
        }
    }

    pub fn new_effect(&self,
                      param_types: Vec<BasicMetadataTypeEnum<'ctx>>,
                      linkage: Option<Linkage>) -> FunctionValue<'ctx> {
        let param_types = param_types.as_slice();

        let func_type = self.context
            .f64_type()
            .fn_type(param_types, false);

        let name = format!(
            "effect@{}",
            self.module.get_name().to_str().unwrap(),
        );

        self.module.add_function(&*name, func_type, linkage)
    }
}
