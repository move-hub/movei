use anyhow::Result;
use move_lang::compiled_unit::CompiledUnit;
use std::{fmt::Debug};


pub trait MoveDialect: Clone + Debug + Sized {
    // const NAME: &'static str;
    fn name(&self) -> &str;
    fn stdlib_files(&self) -> Vec<String>;
}

pub trait Compiler {
    fn compile(&self, text: &str, deps: Vec<String>) -> Result<CompiledUnit>;
}
