use anyhow::Result;
use libra_types::write_set::WriteSet;
use move_core_types::gas_schedule::CostTable;
use move_lang::compiled_unit::CompiledUnit;
use std::fmt::Debug;

pub trait MoveDialect: Clone + Debug + Sized {
    // const NAME: &'static str;
    fn name(&self) -> &str;
    fn stdlib_files(&self) -> Vec<String>;
    fn genesis(&self) -> &WriteSet;
    fn cost_table(&self) -> CostTable;
}

pub trait Compiler {
    fn compile(&self, text: &str, deps: Vec<String>) -> Result<CompiledUnit>;
}
