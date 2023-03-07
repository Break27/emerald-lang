use std::collections::HashMap;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::PointerValue;

pub mod codegen;
pub mod error;

pub struct Compiler<'a, 'ctx> {
    pub context: &'a Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,

    variables: HashMap<String, PointerValue<'ctx>>,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn new(context: &'ctx Context,
               builder: &'a Builder<'ctx>,
               module: &'a Module<'ctx>) -> Self {
        Compiler {
            context,
            builder,
            module,
            variables: HashMap::new()
        }
    }
}
