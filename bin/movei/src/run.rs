pub(crate) mod change_set;
mod diff;
pub(crate) mod exec_context;
mod host_config;
pub mod state;
pub(crate) mod txn_cache;
use crate::{
    hosts::{Config, LibraHost},
    run::exec_context::LocalExecutionContext,
    Run,
};
use anyhow::{bail, Result};
use bytecode_verifier::VerifiedModule;
use libra_types::transaction::TransactionArgument;
use move_core_types::gas_schedule::GasAlgebra;
use move_lang::{compiled_unit::CompiledUnit, shared::Address};
use move_vm_types::{chain_state::ChainState, values::Value};
use std::fs;
use stdlib;
use toml;

pub fn run(args: Run) -> Result<()> {
    let Run {
        config,
        script_name,
        args,
        ..
    } = args;

    let config: Config = toml::from_str(fs::read_to_string(config)?.as_str())?;

    let package = crate::package::get_current_package()?;
    if package.is_none() {
        bail!("unable to get package dir");
    }
    let package = package.unwrap();

    let mut targets = vec![];
    let deps = stdlib::stdlib_files();
    targets.push(
        package
            .script_path(&script_name)
            .to_string_lossy()
            .to_string(),
    );
    targets.push(package.module_path().to_string_lossy().to_string());
    let (sources, compile_units) = move_lang::move_compile_no_report(
        &targets,
        &deps,
        Some(Address::new(config.address.into())),
    )?;

    let compile_units = match compile_units {
        Err(errors) => move_lang::errors::report_errors(sources, errors),
        Ok(units) => units,
    };

    let mut verified_modules = vec![];
    let mut main = None;
    for unit in compile_units {
        let _is_module = match unit {
            CompiledUnit::Module { module, .. } => match VerifiedModule::new(module) {
                Err((m, errs)) => {
                    for e in &errs {
                        println!("{:?} at {:?}", e, m.self_id());
                    }
                }
                Ok(verified_module) => {
                    verified_modules.push(verified_module);
                }
            },
            _ => {
                main = Some(unit.serialize());
            }
        };
    }
    let main_script = main.unwrap();
    let type_args = vec![];

    let run_host = LibraHost::new(config)?;

    let txn_meta = run_host.txn_meta(&main_script, vec![].as_slice(), args.as_slice());
    let gas_schedule = run_host.gas_schedule();
    let state_view = run_host.remote_cache();
    let mut chain_state = LocalExecutionContext::new(txn_meta.max_gas_amount(), state_view);

    let vm = move_vm_runtime::MoveVM::new();
    for m in verified_modules {
        // TODO: handle error
        vm.cache_module(m.clone(), &mut chain_state).unwrap();
    }

    let exec_result = vm.execute_script(
        main_script,
        &gas_schedule,
        &mut chain_state,
        &txn_meta,
        type_args,
        convert_txn_args(&args),
    );

    if let Err(e) = exec_result {
        println!("{:?} when exec {}", &e, &script_name);
    }

    let output = chain_state.make_change_set().unwrap();
    let gas_left = chain_state.remaining_gas();
    let gas_used = txn_meta.max_gas_amount().sub(gas_left);
    println!("gas used: {:?}", gas_used);
    println!("{:#?}", &output);
    Ok(())
}

/// Convert the transaction arguments into move values.
fn convert_txn_args(args: &[TransactionArgument]) -> Vec<Value> {
    args.iter()
        .map(|arg| match arg {
            TransactionArgument::U64(i) => Value::u64(*i),
            TransactionArgument::Address(a) => Value::address(*a),
            TransactionArgument::Bool(b) => Value::bool(*b),
            TransactionArgument::U8Vector(v) => Value::vector_u8(v.clone()),
        })
        .collect()
}
