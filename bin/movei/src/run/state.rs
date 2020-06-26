use anyhow::Result;
use libra_state_view::StateView;
use libra_types::{
    access_path::AccessPath,
    write_set::{WriteOp, WriteSet},
};
use move_core_types::language_storage::ModuleId;
use move_vm_runtime::data_cache::RemoteCache;
use std::collections::HashMap;
use vm::{errors::VMResult, CompiledModule};

/// An in-memory implementation of [`RemoteCache`] for the VM.
///
/// Tests use this to set up state, and pass in a reference to the cache whenever a
/// `RemoteCache` is needed.
/// Modified from Libra e2e-tests
#[derive(Debug, Default)]
pub struct FakeDataStore {
    data: HashMap<AccessPath, Vec<u8>>,
}

impl FakeDataStore {
    /// Creates a new `FakeDataStore` with the provided initial data.
    pub fn new(data: HashMap<AccessPath, Vec<u8>>) -> Self {
        FakeDataStore { data }
    }

    /// Adds a [`WriteSet`] to this data store.
    pub fn add_write_set(&mut self, write_set: &WriteSet) {
        for (access_path, write_op) in write_set {
            match write_op {
                WriteOp::Value(blob) => {
                    self.set(access_path.clone(), blob.clone());
                }
                WriteOp::Deletion => {
                    self.remove(access_path);
                }
            }
        }
    }

    /// Sets a (key, value) pair within this data store.
    ///
    /// Returns the previous data if the key was occupied.
    pub fn set(&mut self, access_path: AccessPath, data_blob: Vec<u8>) -> Option<Vec<u8>> {
        self.data.insert(access_path, data_blob)
    }

    /// Deletes a key from this data store.
    ///
    /// Returns the previous data if the key was occupied.
    pub fn remove(&mut self, access_path: &AccessPath) -> Option<Vec<u8>> {
        self.data.remove(access_path)
    }

    /// Adds a [`CompiledModule`] to this data store.
    ///
    /// Does not do any sort of verification on the module.
    pub fn add_module(&mut self, module_id: &ModuleId, module: &CompiledModule) {
        let access_path = AccessPath::from(module_id);
        let mut blob = vec![];
        module
            .serialize(&mut blob)
            .expect("serializing this module should work");
        self.set(access_path, blob);
    }
}

impl StateView for FakeDataStore {
    fn get(&self, access_path: &AccessPath) -> Result<Option<Vec<u8>>> {
        // Since the data is in-memory, it can't fail.
        Ok(self.data.get(access_path).cloned())
    }

    fn multi_get(&self, _access_paths: &[AccessPath]) -> Result<Vec<Option<Vec<u8>>>> {
        unimplemented!();
    }

    fn is_genesis(&self) -> bool {
        self.data.is_empty()
    }
}

// This is used by the `process_transaction` API.
impl RemoteCache for FakeDataStore {
    fn get(&self, access_path: &AccessPath) -> VMResult<Option<Vec<u8>>> {
        Ok(self.data.get(access_path).cloned())
    }
}
