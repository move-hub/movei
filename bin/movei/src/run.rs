#![allow(dead_code)]

mod diff;
mod host_config;

use crate::{context::MoveiContext, run::diff::print_diff};
use anyhow::{Error, Result};
use clap::Clap;
use dialect::MoveDialect;
use libra_types::{account_address::AccountAddress, transaction::TransactionArgument};
use move_core_types::{
    gas_schedule::{GasAlgebra, GasUnits},
    language_storage::TypeTag,
    parser::parse_transaction_argument,
};
use move_lang::{compiled_unit::CompiledUnit, shared::Address};
use move_vm_runtime::{
    data_cache::{RemoteCache, TransactionEffects},
    move_vm::MoveVM,
};
use move_vm_types::{gas_schedule::CostStrategy, values::Value};
use movei_executor::{output_formatter::to_changes, state::FakeDataStore};
use resource_viewer::MoveValueAnnotator;
use std::{collections::HashMap, convert::TryFrom};
use vm::errors::VMResult;

#[derive(Clap, Debug)]
pub struct RunArgs {
    #[clap(
    name = "SENDER",
    long = "sender",
    about = "address executing the script",
    parse(try_from_str=AccountAddress::from_hex_literal)
    )]
    pub sender: AccountAddress,

    #[clap(long = "no-std", about = "exec without std")]
    pub no_std: bool,

    #[clap(name = "script_name", about = "script to run")]
    pub script_name: String,

    #[clap(
    name = "arg",
    about = "script arguments",
    multiple = true,
    parse(try_from_str=parse_transaction_argument)
    )]
    pub args: Vec<TransactionArgument>,
}

pub fn run(args: RunArgs, context: MoveiContext) -> Result<()> {
    let RunArgs {
        sender,
        no_std,
        script_name,
        args,
    } = args;
    let package = context.package();
    let dialect = context.dialect();
    let mut targets = vec![];
    let mut deps = vec![];
    targets.push(
        package
            .script_path(&script_name)
            .to_string_lossy()
            .to_string(),
    );
    targets.push(package.module_dir().to_string_lossy().to_string());
    if !no_std {
        deps.extend(dialect.stdlib_files());
    }
    let (sources, compile_units) = move_lang::move_compile_no_report(
        &targets,
        &deps,
        Some(Address::try_from(sender.as_ref()).unwrap()),
    )?;

    let compile_units = match compile_units {
        Err(errors) => move_lang::errors::report_errors(sources, errors),
        Ok(units) => units,
    };

    let mut compiled_modules = vec![];
    let mut main = None;
    for unit in compile_units {
        let _is_module = match unit {
            CompiledUnit::Module { module, .. } => compiled_modules.push(module),

            _ => {
                main = Some(unit.serialize());
            }
        };
    }
    let dialect = context.dialect();

    let mut data_store = FakeDataStore::new(HashMap::new());
    data_store.add_write_set(dialect.genesis());
    // cache modules
    for compiled_module in compiled_modules.iter() {
        data_store.add_module(&compiled_module.self_id(), compiled_module);
    }

    let cost_table = dialect.cost_table();
    // let mut chain_state = TransactionDataCache::new(&data_store);
    let mut cost_strategy = CostStrategy::transaction(&cost_table, GasUnits::new(u64::MAX));

    let exec_result = {
        let main_script = main.unwrap();
        let type_args = vec![];
        let args = convert_txn_args(&args);

        exec_script(
            main_script,
            type_args,
            args,
            sender,
            &data_store,
            &mut cost_strategy,
        )
    };

    if let Err(e) = exec_result {
        println!("{:?} when exec {}", &e, &script_name);

        return Err(Error::from(e.into_vm_status()));
    }
    let txn_effects = exec_result.unwrap();
    let gas_used = u64::MAX - cost_strategy.remaining_gas().get();
    // let events = chain_state.event_data().to_vec();

    println!("ChangeSet:");
    let annotator = MoveValueAnnotator::new(&data_store);
    let changes = to_changes(txn_effects);
    print_diff(changes, annotator)?;
    println!("GasUsed: {}", gas_used);
    Ok(())
}

/// Convert the transaction arguments into move values.
fn convert_txn_args(args: &[TransactionArgument]) -> Vec<Value> {
    args.iter()
        .map(|arg| match arg {
            TransactionArgument::U8(i) => Value::u8(*i),
            TransactionArgument::U64(i) => Value::u64(*i),
            TransactionArgument::U128(i) => Value::u128(*i),
            TransactionArgument::Address(a) => Value::address(*a),
            TransactionArgument::Bool(b) => Value::bool(*b),
            TransactionArgument::U8Vector(v) => Value::vector_u8(v.clone()),
        })
        .collect()
}

fn exec_script<'a, R: RemoteCache>(
    script: Vec<u8>,
    ty_args: Vec<TypeTag>,
    args: Vec<Value>,
    sender: AccountAddress,
    data_store: &'a R,
    cost_strategy: &'a mut CostStrategy,
) -> VMResult<TransactionEffects> {
    let vm = MoveVM::new();
    let mut session = vm.new_session(data_store);

    session.execute_script(script, ty_args, args, vec![sender], cost_strategy)?;
    session.finish()
}
