use compiled_stdlib::StdLibOptions;
use dialect::MoveDialect;
use libra_types::{
    access_path::AccessPath,
    on_chain_config::{ConfigStorage, OnChainConfig, VMConfig},
    transaction::ChangeSet,
    write_set::*,
};
use move_core_types::gas_schedule::CostTable;
use once_cell::sync::Lazy;
use stdlib::stdlib_files;
use vm_genesis::generate_genesis_change_set_for_testing;

/// Dummy genesis ChangeSet for testing
pub static GENESIS_CHANGE_SET: Lazy<ChangeSet> =
    Lazy::new(|| generate_genesis_change_set_for_testing(StdLibOptions::Compiled));

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct LibraDialect {
    genesis: ChangeSet,
}
impl Default for LibraDialect {
    fn default() -> Self {
        Self {
            genesis: GENESIS_CHANGE_SET.clone(),
        }
    }
}
impl LibraDialect {
    pub fn new() -> Self {
        Self::default()
    }
}

impl<'a> ConfigStorage for &'a LibraDialect {
    fn fetch_config(&self, access_path: AccessPath) -> Option<Vec<u8>> {
        for (ap, w) in self.genesis.write_set().iter() {
            if ap == &access_path {
                return match w {
                    WriteOp::Value(v) => Some(v.clone()),
                    WriteOp::Deletion => None,
                };
            }
        }
        None
    }
}

const NAME: &str = "Libra";
impl MoveDialect for LibraDialect {
    fn name(&self) -> &str {
        NAME
    }
    fn stdlib_files(&self) -> Vec<String> {
        stdlib_files()
    }

    fn genesis(&self) -> &WriteSet {
        self.genesis.write_set()
    }

    fn cost_table(&self) -> CostTable {
        let c = VMConfig::fetch_config(&self).expect("vm config should exists in genesis");
        c.gas_schedule
    }
}
