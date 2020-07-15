use libra_types::account_address::AccountAddress;
use move_core_types::{
    language_storage::{ModuleId, StructTag},
    value::MoveStructLayout,
};
use move_vm_types::values::Struct;
use std::collections::BTreeMap;

#[derive(Debug, Default)]
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

#[derive(Debug, Default)]
pub struct ChangeSetMut {
    changes: BTreeMap<AccountAddress, Vec<Change>>,
}

impl ChangeSetMut {
    pub fn add_change(&mut self, address: AccountAddress, change: Change) {
        self.changes
            .entry(address)
            .or_insert_with(Vec::new)
            .push(change);
    }

    pub fn freeze(self) -> ChangeSet {
        // TODO: add structural validation
        self.into()
    }
}
#[derive(Debug)]
pub enum Change {
    DeleteResource(StructTag),
    ModifyResource(StructTag, MoveStructLayout, Struct),
    AddModule(ModuleId, Vec<u8>),
}
