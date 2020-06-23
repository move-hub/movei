use bytecode_verifier::VerifiedModule;
use libra_types::{account_address::AccountAddress, language_storage::ModuleId};
use move_core_types::identifier;
use std::collections::BTreeMap;
use vm::{
    access::ModuleAccess,
    file_format::{self},
};

pub trait StructFieldResolver {
    fn resolve_field(
        &self,
        module_address: &AccountAddress,
        module_name: &identifier::IdentStr,
        struct_name: &identifier::IdentStr,
        idx: usize,
    ) -> Option<&identifier::IdentStr>;
}

pub struct LoadedModules {
    modules: BTreeMap<ModuleId, VerifiedModule>,
}

impl LoadedModules {
    pub fn new(modules: Vec<VerifiedModule>) -> Self {
        let modules = modules.into_iter().map(|m| (m.self_id(), m)).collect();
        Self { modules }
    }
}

impl StructFieldResolver for LoadedModules {
    fn resolve_field(
        &self,
        module_address: &AccountAddress,
        module_name: &identifier::IdentStr,
        struct_name: &identifier::IdentStr,
        idx: usize,
    ) -> Option<&identifier::IdentStr> {
        let module_id = ModuleId::new(*module_address, module_name.to_owned());
        let m = self.modules.get(&module_id)?;
        for d in m.struct_defs() {
            let struct_handle = m.struct_handle_at(d.struct_handle);
            let id_idx = struct_handle.name;
            if m.identifier_at(id_idx).as_str() == struct_name.as_str() {
                return match &d.field_information {
                    file_format::StructFieldInformation::Native => None,
                    file_format::StructFieldInformation::Declared(fs) => {
                        let name_index = fs.get(idx as usize)?.name;
                        Some(m.identifier_at(name_index))
                    }
                };
            }
        }

        None
    }
}
