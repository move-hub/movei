use crate::run::change_set::{Change, ChangeSet, ChangeSetMut};
use libra_types::{
    access_path::AccessPath,
    language_storage::ModuleId,
    vm_error::{StatusCode, VMStatus},
};
use move_vm_state::data_cache::RemoteCache;
use move_vm_types::{
    loaded_data::types::FatStructType,
    values::{GlobalValue, Struct, Value},
};
use std::collections::BTreeMap;
use vm::errors::{vm_error, Location, VMResult};

/// NOTICE: the code is copied from libra.
/// some changes are made to track the resource type of accessed path.
pub struct TransactionDataCache<'txn> {
    // TODO: an AccessPath corresponds to a top level resource but that may not be the
    // case moving forward, so we need to review this.
    // Also need to relate this to a ResourceKey.
    data_map: BTreeMap<AccessPath, Option<GlobalValue>>,
    // if a resource is deleted, when generating write set,
    // the struct_type of that resource cannot be known.
    // This field is used to keep track of it.
    data_types: BTreeMap<AccessPath, FatStructType>,
    // keep track of loaded data.
    origin_data_map: BTreeMap<AccessPath, Struct>,
    module_map: BTreeMap<ModuleId, Vec<u8>>,
    data_cache: &'txn dyn RemoteCache,
}

impl<'txn> TransactionDataCache<'txn> {
    pub fn new(data_cache: &'txn dyn RemoteCache) -> Self {
        TransactionDataCache {
            data_cache,
            data_map: BTreeMap::new(),
            data_types: BTreeMap::new(),
            origin_data_map: BTreeMap::new(),
            module_map: BTreeMap::new(),
        }
    }

    pub fn exists_module(&self, m: &ModuleId) -> bool {
        self.module_map.contains_key(m) || {
            let ap = AccessPath::from(m);
            matches!(self.data_cache.get(&ap), Ok(Some(_)))
        }
    }

    pub fn load_module(&self, module: &ModuleId) -> VMResult<Vec<u8>> {
        match self.module_map.get(module) {
            Some(bytes) => Ok(bytes.clone()),
            None => {
                let ap = AccessPath::from(module);
                self.data_cache.get(&ap).and_then(|data| {
                    data.ok_or_else(|| {
                        VMStatus::new(StatusCode::LINKER_ERROR)
                            .with_message(format!("Cannot find {:?} in data cache", module))
                    })
                })
            }
        }
    }

    pub fn publish_module(&mut self, m: ModuleId, bytes: Vec<u8>) -> VMResult<()> {
        self.module_map.insert(m, bytes);
        Ok(())
    }

    pub fn publish_resource(
        &mut self,
        ap: &AccessPath,
        g: (FatStructType, GlobalValue),
    ) -> VMResult<()> {
        self.data_map.insert(ap.clone(), Some(g.1));
        self.data_types.insert(ap.clone(), g.0);
        Ok(())
    }

    // Retrieve data from the local cache or loads it from the remote cache into the local cache.
    // All operations on the global data are based on this API and they all load the data
    // into the cache.
    // TODO: this may not be the most efficient model because we always load data into the
    // cache even when that would not be strictly needed. Review once we have the whole story
    // working
    pub(crate) fn load_data(
        &mut self,
        ap: &AccessPath,
        ty: &FatStructType,
    ) -> VMResult<&mut Option<GlobalValue>> {
        if !self.data_map.contains_key(ap) {
            match self.data_cache.get(ap)? {
                Some(bytes) => {
                    let res = Struct::simple_deserialize(&bytes, ty)?;
                    // deserialize twice, because Value cannot be cloned.
                    let res_copy = Struct::simple_deserialize(&bytes, ty)?;
                    let gr = GlobalValue::new(Value::struct_(res))?;
                    self.origin_data_map.insert(ap.clone(), res_copy);
                    self.data_map.insert(ap.clone(), Some(gr));
                    self.data_types.insert(ap.clone(), ty.clone());
                }
                None => {
                    return Err(
                        VMStatus::new(StatusCode::MISSING_DATA).with_message(format!(
                            "Cannot find {:?}::{}::{} for Access Path: {:?}",
                            &ty.address,
                            &ty.module.as_str(),
                            &ty.name.as_str(),
                            ap
                        )),
                    );
                }
            };
        }
        Ok(self.data_map.get_mut(ap).expect("data must exist"))
    }

    /// Make a change set from the updated (dirty, deleted) global resources along with
    /// to-be-published modules.
    /// Consume the TransactionDataCache and must be called at the end of a transaction.
    /// This also ends up checking that reference count around global resources is correct
    /// at the end of the transactions (all ReleaseRef are properly called)
    pub fn make_change_set(&mut self) -> VMResult<ChangeSet> {
        if self.data_map.len() + self.module_map.len() > usize::max_value() {
            return Err(vm_error(Location::new(), StatusCode::INVALID_DATA));
        }

        let data_map = std::mem::replace(&mut self.data_map, BTreeMap::new());
        let mut data_types = std::mem::replace(&mut self.data_types, BTreeMap::new());
        let mut old_data_map = std::mem::replace(&mut self.origin_data_map, BTreeMap::new());

        let mut changes = ChangeSetMut::new();

        for (key, global_val) in data_map {
            let layout = data_types.remove(&key).unwrap();
            let old_data = old_data_map.remove(&key);
            match global_val {
                Some(global_val) => {
                    if !global_val.is_clean()? {
                        // into_owned_struct will check if all references are properly released
                        // at the end of a transaction
                        let data = global_val.into_owned_struct()?;

                        let change = match old_data {
                            Some(o) => Change::ModifyResource(layout, o, data),
                            None => Change::AddResource(layout, data),
                        };
                        changes.add_change(key.address, change);
                    }
                }
                None => {
                    if old_data.is_none() {
                        return Err(vm_error(Location::new(), StatusCode::INVALID_DATA));
                    }
                    changes.add_change(
                        key.address,
                        Change::DeleteResource(layout, old_data.unwrap()),
                    );
                }
            }
        }

        let module_map = std::mem::replace(&mut self.module_map, BTreeMap::new());
        for (module_id, module) in module_map {
            changes.add_change(
                module_id.address().clone(),
                Change::AddModule(module_id.clone(), module),
            );
        }

        changes
            .freeze()
            .map_err(|_| vm_error(Location::new(), StatusCode::DATA_FORMAT_ERROR))
    }

    /// Flush out the cache and restart from a clean state
    pub fn clear(&mut self) {
        self.data_map.clear();
        self.data_types.clear();
        self.module_map.clear();
    }
}
