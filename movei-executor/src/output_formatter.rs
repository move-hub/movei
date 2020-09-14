use crate::change_set::{Change, ChangeSet, ChangeSetMut};
use move_core_types::value::MoveTypeLayout;
use move_vm_runtime::data_cache::TransactionEffects;
use move_vm_types::values::VMValueCast;

pub fn to_changes(txn_effects: TransactionEffects) -> ChangeSet {
    let TransactionEffects {
        modules,
        resources,
        events: _,
    } = txn_effects;

    let mut change_set = ChangeSetMut::default();
    for (addr, resource_changes) in resources {
        for (struct_tag, value) in resource_changes {
            let change = match value {
                Some((layout, v)) => {
                    let layout = match layout {
                        MoveTypeLayout::Struct(l) => l,
                        _ => unreachable!(),
                    };
                    Change::ModifyResource(struct_tag, layout, v.cast().unwrap())
                }
                None => Change::DeleteResource(struct_tag),
            };
            change_set.add_change(addr, change);
        }
    }
    for (module_id, data) in modules {
        change_set.add_change(*module_id.address(), Change::AddModule(module_id, data));
    }
    change_set.freeze()
}
