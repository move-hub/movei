use super::txn_cache::TransactionDataCache;
use crate::run::change_set::ChangeSet;
use libra_types::{
    access_path::AccessPath,
    contract_event::ContractEvent,
    language_storage::ModuleId,
    vm_error::{StatusCode, VMStatus},
};
use move_core_types::gas_schedule::{GasAlgebra, GasCarrier, GasUnits};
use move_vm_state::data_cache::RemoteCache;
use move_vm_types::{
    chain_state::ChainState, loaded_data::types::FatStructType, values::GlobalValue,
};
use vm::errors::VMResult;

/// A LocalExecutionContext holds the mutable data that needs to be persisted from one
/// section of the transaction flow to another.
pub struct LocalExecutionContext<'txn> {
    /// Gas metering to track cost of execution.
    gas_left: GasUnits<GasCarrier>,
    /// List of events "fired" during the course of an execution.
    event_data: Vec<ContractEvent>,
    /// Data store
    data_view: TransactionDataCache<'txn>,
}

impl<'txn> LocalExecutionContext<'txn> {
    pub fn new(gas_left: GasUnits<GasCarrier>, data_cache: &'txn dyn RemoteCache) -> Self {
        Self {
            gas_left,
            event_data: Vec::new(),
            data_view: TransactionDataCache::new(data_cache),
        }
    }

    pub fn events(&self) -> &[ContractEvent] {
        &self.event_data
    }

    pub fn make_change_set(&mut self) -> VMResult<ChangeSet> {
        self.data_view.make_change_set()
    }

    pub fn clear(&mut self) {
        self.data_view.clear();
        self.event_data.clear();
    }
}

impl<'txn> ChainState for LocalExecutionContext<'txn> {
    fn deduct_gas(&mut self, amount: GasUnits<GasCarrier>) -> VMResult<()> {
        if self
            .gas_left
            .app(&amount, |curr_gas, gas_amt| curr_gas >= gas_amt)
        {
            self.gas_left = self.gas_left.sub(amount);
            Ok(())
        } else {
            // Zero out the internal gas state
            self.gas_left = GasUnits::new(0);
            Err(VMStatus::new(StatusCode::OUT_OF_GAS))
        }
    }

    fn remaining_gas(&self) -> GasUnits<GasCarrier> {
        self.gas_left
    }

    fn load_module(&self, module: &ModuleId) -> VMResult<Vec<u8>> {
        self.data_view.load_module(module)
    }

    fn borrow_resource(
        &mut self,
        ap: &AccessPath,
        ty: &FatStructType,
    ) -> VMResult<Option<&GlobalValue>> {
        let map_entry = self.data_view.load_data(ap, ty)?;
        Ok(map_entry.as_ref())
    }

    fn move_resource_from(
        &mut self,
        ap: &AccessPath,
        ty: &FatStructType,
    ) -> VMResult<Option<GlobalValue>> {
        let map_entry = self.data_view.load_data(ap, ty)?;
        // .take() means that the entry is removed from the data map -- this marks the
        // access path for deletion.
        Ok(map_entry.take())
    }

    fn publish_module(&mut self, module_id: ModuleId, module: Vec<u8>) -> VMResult<()> {
        self.data_view.publish_module(module_id, module)
    }

    fn publish_resource(
        &mut self,
        ap: &AccessPath,
        g: (FatStructType, GlobalValue),
    ) -> VMResult<()> {
        self.data_view.publish_resource(ap, g)
    }

    fn exists_module(&self, key: &ModuleId) -> bool {
        self.data_view.exists_module(key)
    }

    fn emit_event(&mut self, event: ContractEvent) {
        self.event_data.push(event)
    }
}
