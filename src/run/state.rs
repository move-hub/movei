use libra_types::access_path::AccessPath;
use move_vm_state::{data_cache::RemoteCache};
use vm::errors::VMResult;

pub struct FakeDataStore;
impl RemoteCache for FakeDataStore {
    fn get(&self, _access_path: &AccessPath) -> VMResult<Option<Vec<u8>>> {
        return Ok(None);
    }
}
