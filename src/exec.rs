#![allow(dead_code)]

use crate::run::state::FakeDataStore;
use anyhow::{bail, Result};
use bytecode_verifier::VerifiedModule;
use clap::Clap;
use libra_types::{
    account_address::AccountAddress,
    transaction::{parse_as_transaction_argument, TransactionArgument},
};
use move_lang::{command_line::parse_address, compiled_unit::CompiledUnit, shared::Address};
use move_vm_types::values::Value;
use vm::{gas_schedule::zero_cost_schedule, transaction_metadata::TransactionMetadata};

#[derive(Clap, Debug)]
pub struct ExecArgs {
    #[clap(
        name = "SENDER",
        long = "sender",
        help = "address executing the script",
        parse(try_from_str=parse_address)
    )]
    pub sender: Address,

    #[clap(long = "no-std", help = "exec without std")]
    pub no_std: bool,

    #[clap(
        name = "script_name",
        short = "s",
        long = "script",
        help = "script to run"
    )]
    pub script_name: String,

    #[clap(
    name = "arg",
    short = "a",
    long = "arg",
    help = "script arguments",
    parse(try_from_str=parse_as_transaction_argument),
    multiple = true
    )]
    pub args: Vec<TransactionArgument>,
}

pub fn run(args: ExecArgs) -> Result<()> {
    let ExecArgs {
        sender,
        no_std,
        script_name,
        args,
    } = args;
    let package = crate::package::get_current_package()?;
    if package.is_none() {
        bail!("unable to get package dir");
    }
    let package = package.unwrap();
    let mut targets = vec![];
    let deps = vec![];
    targets.push(
        package
            .script_path(&script_name)
            .to_string_lossy()
            .to_string(),
    );
    targets.push(package.module_path().to_string_lossy().to_string());
    if !no_std {
        targets.extend(stdlib::stdlib_files());
    }
    let (sources, compile_units) =
        move_lang::move_compile_no_report(&targets, &deps, Some(sender))?;

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

    let vm = move_vm_runtime::MoveVM::new();
    let data_store = FakeDataStore;
    let mut txn_meta = TransactionMetadata::default();
    txn_meta.sender = AccountAddress::new(sender.to_u8());
    let mut chain_state = super::run::exec_context::LocalExecutionContext::new(
        txn_meta.max_gas_amount(),
        &data_store,
    );
    for verified_module in verified_modules {
        vm.cache_module(verified_module, &mut chain_state).unwrap();
    }

    let main_script = main.unwrap();
    let type_args = vec![];
    let args = convert_txn_args(&args);
    let exec_result = vm.execute_script(
        main_script,
        &zero_cost_schedule(),
        &mut chain_state,
        &txn_meta,
        type_args,
        args,
    );
    if let Err(e) = exec_result {
        println!("{:?} when exec {}", &e, &script_name);
    } else {
        let output = chain_state.make_change_set().unwrap();
        println!("{:#?}", &output);
    }

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
