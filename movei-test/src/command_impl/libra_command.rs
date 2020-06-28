use crate::command::{Command, CommandConfigEntry};
use anyhow::{bail, format_err, Error, Result};
use functional_tests::{
    config::{block_metadata, global, transaction},
    preprocessor::RawTransactionInput,
};
use std::{
    fmt::{Debug, Display, Formatter},
    ops::{Deref, DerefMut},
    str::FromStr,
};

#[derive(Debug)]
pub enum LibraTestCommand {
    GlobalConfig(global::Entry),
    BlockMeta(Vec<block_metadata::Entry>),
    Transaction(TransactionCommand),
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
            .map(|e| ConfigEntry::BlockMeta(e))
            .or_else(|_| s.parse().map(|e| ConfigEntry::Transaction(e)))
            .map_err(|_| format_err!("failed to parse '{}' as config entry", s))?;
        Ok(parsed)
    }
}

impl CommandConfigEntry for ConfigEntry {}

impl Command for LibraTestCommand {
    type ConfigEntry = ConfigEntry;

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
