use crate::{
    command::{Command, CommandConfigEntry, CommandEvaluator, EvaluationLog},
    move_test_compiler::MoveSourceCompiler,
};
use anyhow::{bail, format_err, Error, Result};
use dialect::MoveDialect;
use functional_tests::{
    config::{block_metadata, global, transaction},
    evaluator as libra_evaluator,
    preprocessor::{substitute_addresses_and_auth_keys, RawTransactionInput},
};
use itertools::Itertools;

use movei_libra_dialect::LibraDialect;

use std::{
    fmt::{Debug, Display},
    ops::{Deref, DerefMut},
    str::FromStr,
};
#[derive(Debug)]
pub enum LibraTestCommand {
    GlobalConfig(global::Entry),
    BlockMeta(Vec<block_metadata::Entry>),
    Transaction(TransactionCommand),
}

impl LibraTestCommand {
    pub fn is_global_config(&self) -> bool {
        matches!(self, LibraTestCommand::GlobalConfig(_))
    }
}

impl Default for LibraTestCommand {
    fn default() -> Self {
        LibraTestCommand::Transaction(TransactionCommand::default())
    }
}

pub struct TransactionCommand {
    inner: RawTransactionInput,
}

impl Default for TransactionCommand {
    fn default() -> Self {
        Self {
            inner: RawTransactionInput {
                config_entries: vec![],
                text: vec![],
            },
        }
    }
}
impl Debug for TransactionCommand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "TransactionCommand(config: {:?}, text: {:?})",
            &self.inner.config_entries, &self.inner.text
        )
    }
}

impl Deref for TransactionCommand {
    type Target = RawTransactionInput;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
impl DerefMut for TransactionCommand {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl Display for LibraTestCommand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let display = match self {
            LibraTestCommand::GlobalConfig(_) => "global",
            LibraTestCommand::BlockMeta(_) => "block",
            LibraTestCommand::Transaction(_) => "txn",
        };
        write!(f, "{}", display)
    }
}

impl FromStr for LibraTestCommand {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let command = if let Ok(parsed) = s.parse::<global::Entry>() {
            LibraTestCommand::GlobalConfig(parsed)
        } else if transaction::is_new_transaction(s) {
            LibraTestCommand::Transaction(TransactionCommand::default())
        } else if block_metadata::is_new_block(s) {
            LibraTestCommand::BlockMeta(vec![])
        } else {
            bail!("failed to parse '{}' as config entry", s);
        };

        Ok(command)
    }
}

#[derive(Debug)]
pub enum ConfigEntry {
    BlockMeta(block_metadata::Entry),
    Transaction(transaction::Entry),
}
impl FromStr for ConfigEntry {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parsed = s
            .parse::<block_metadata::Entry>()
            .map(ConfigEntry::BlockMeta)
            .or_else(|_| s.parse().map(ConfigEntry::Transaction))
            .map_err(|_| format_err!("failed to parse '{}' as config entry", s))?;
        Ok(parsed)
    }
}

impl CommandConfigEntry for ConfigEntry {}

impl Command for LibraTestCommand {
    type ConfigEntry = ConfigEntry;

    fn has_config(&self) -> bool {
        match self {
            LibraTestCommand::GlobalConfig(_) => false,
            _ => true,
        }
    }
    fn add_config(&mut self, config: Self::ConfigEntry) -> Result<()> {
        match (self, config) {
            (LibraTestCommand::BlockMeta(meta), ConfigEntry::BlockMeta(entry)) => {
                meta.push(entry);
            }
            (LibraTestCommand::Transaction(txn_input), ConfigEntry::Transaction(entry)) => {
                txn_input.config_entries.push(entry)
            }
            (command, entry) => {
                bail!("invalid config {:?} in {} command context", entry, command);
            }
        }
        Ok(())
    }

    fn add_textline(&mut self, line: &str) -> Result<()> {
        match self {
            LibraTestCommand::Transaction(txn_input) => {
                txn_input.text.push(line.to_string());
            }
            command => {
                bail!("{} don't need text data", command);
            }
        }
        Ok(())
    }

    fn validate(&self) -> Result<()> {
        match self {
            LibraTestCommand::Transaction(txn) => {
                if txn.text.is_empty() {
                    if !txn.config_entries.is_empty() {
                        bail!("config options attached to empty transaction");
                    }
                    bail!("empty transaction");
                }
            }
            LibraTestCommand::BlockMeta(entries) => {
                if entries.len() < 2 {
                    bail!("block prologue doesn't have enough arguments");
                }
            }
            _ => {}
        }
        Ok(())
    }
}

pub struct LibraCommandEvaluator {
    // store: FakeDataStore,
    dialect: LibraDialect,
}

impl LibraCommandEvaluator {
    // pub fn new() -> Self {
    //     let dialect = LibraDialect::new();
    //     let genesis = dialect.genesis();
    //     let mut store = FakeDataStore::default();
    //     store.add_write_set(genesis);
    //     let mut global_config = GloablConfig {
    //         rng: StdRng::from_seed([0x1f; 32]),
    //         config: global::Config {
    //             accounts: BTreeMap::new(),
    //             genesis_accounts: BTreeMap::new(),
    //             addresses: BTreeMap::new(),
    //             validator_accounts: 0,
    //         },
    //     };
    //
    //     Self {
    //         store,
    //         dialect,
    //         config: global_config,
    //     }
    // }
    pub fn new() -> Self {
        Self {
            dialect: LibraDialect::new(),
        }
    }
}

impl CommandEvaluator for LibraCommandEvaluator {
    type Cmd = LibraTestCommand;

    fn eval(&self, mut commands: Vec<Self::Cmd>) -> Result<EvaluationLog, Error> {
        let configs: Vec<_> = commands.drain_filter(|c| c.is_global_config()).collect();
        let configs: Vec<_> = configs
            .into_iter()
            .filter_map(|c| match c {
                LibraTestCommand::GlobalConfig(e) => Some(e),
                _ => None,
            })
            .collect();
        let global_config = global::Config::build(configs.as_ref())?;
        let mut libra_commands = Vec::with_capacity(commands.len());
        for c in commands {
            let libra_cmd = match c {
                LibraTestCommand::BlockMeta(es) => libra_evaluator::Command::BlockMetadata(
                    block_metadata::build_block_metadata(&global_config, es.as_slice())?,
                ),
                LibraTestCommand::Transaction(t) => {
                    let input = t
                        .text
                        .iter()
                        .map(|l| substitute_addresses_and_auth_keys(&global_config, l.as_str()))
                        .join("\n");
                    libra_evaluator::Command::Transaction(libra_evaluator::Transaction {
                        config: transaction::Config::build(&global_config, &t.config_entries)?,
                        input,
                    })
                }
                _ => unreachable!(),
            };
            libra_commands.push(libra_cmd);
        }

        let compiler = MoveSourceCompiler::new(self.dialect.stdlib_files());
        let logs = libra_evaluator::eval(&global_config, compiler, libra_commands.as_slice())?;
        Ok(logs)
    }
}
