use anyhow::Result;
use libra_types::{account_address::AccountAddress, language_storage::ModuleId};
use move_vm_types::{loaded_data::types::FatStructType, values::Struct};
use std::collections::BTreeMap;

#[derive(Debug)]
pub struct ChangeSet {
    changes: BTreeMap<AccountAddress, Vec<Change>>,
}

impl ChangeSet {
    pub fn into_changes(self) -> BTreeMap<AccountAddress, Vec<Change>> {
        self.changes
    }
}

impl From<ChangeSetMut> for ChangeSet {
    fn from(c: ChangeSetMut) -> Self {
        Self { changes: c.changes }
    }
}

#[derive(Debug)]
pub struct ChangeSetMut {
    changes: BTreeMap<AccountAddress, Vec<Change>>,
}

impl ChangeSetMut {
    pub fn new() -> Self {
        Self {
            changes: BTreeMap::new(),
        }
    }
}

impl ChangeSetMut {
    pub fn delete_resource(
        &mut self,
        _address: AccountAddress,
        _layout: FatStructType,
        _value: Struct,
    ) {
        todo!()
    }
    pub fn add_resource(
        &mut self,
        _address: AccountAddress,
        _layout: FatStructType,
        _value: Struct,
    ) {
        todo!()
    }
    pub fn modify_resource(
        &mut self,
        _address: AccountAddress,
        _layout: FatStructType,
        _old_value: Struct,
        _new_value: Struct,
    ) {
        todo!()
    }

    pub fn add_module(&mut self, _address: AccountAddress, _module_id: ModuleId, _code: Vec<u8>) {
        todo!()
    }

    pub fn add_change(&mut self, address: AccountAddress, change: Change) {
        self.changes.entry(address).or_insert(vec![]).push(change);
    }

    pub fn freeze(self) -> Result<ChangeSet> {
        // TODO: add structural validation
        Ok(self.into())
    }
}
#[derive(Debug)]
pub enum Change {
    DeleteResource(FatStructType, Struct),
    AddResource(FatStructType, Struct),
    ModifyResource(FatStructType, Struct, Struct),
    AddModule(ModuleId, Vec<u8>),
}
