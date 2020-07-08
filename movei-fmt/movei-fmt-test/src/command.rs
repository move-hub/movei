use anyhow::{bail, ensure, Result};
use movei_test::{Command, CommandConfigEntry};
use std::str::FromStr;

#[derive(Debug, Clone)]
pub enum ConfigEntry {
    Width(u64),
    Indent(usize),
}

impl FromStr for ConfigEntry {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let remaining = strip(s.trim(), "//!");

        let trimed = remaining
            .as_ref()
            .and_then(|l| strip(l.trim_start(), "width:"));
        if let Some(trimed) = trimed {
            let width = trimed.trim().parse::<u64>()?;
            return Ok(ConfigEntry::Width(width));
        }

        let trimed = remaining
            .as_ref()
            .and_then(|l| strip(l.trim_start(), "indent:"));
        if let Some(trimed) = trimed {
            let indent = trimed.trim().parse::<usize>()?;
            return Ok(ConfigEntry::Indent(indent));
        }

        bail!("cannot parse to width config")
    }
}
impl CommandConfigEntry for ConfigEntry {}

/// Checks if `s` starts with `prefix`. If yes, returns a reference to the remaining part
/// with the prefix stripped away.
pub fn strip<'a>(s: &'a str, prefix: &str) -> Option<&'a str> {
    if s.starts_with(prefix) {
        Some(&s[prefix.len()..])
    } else {
        None
    }
}

#[derive(Debug)]
pub enum FmtTestCommand {
    Fmt(FmtConfig),
}
impl Default for FmtTestCommand {
    fn default() -> Self {
        Self::Fmt(Default::default())
    }
}

#[derive(Debug)]
pub struct FmtConfig {
    width: u64,
    indent: usize,
    text: Vec<String>,
}

impl Default for FmtConfig {
    fn default() -> Self {
        Self {
            width: 80,
            indent: 4,
            text: vec![],
        }
    }
}
impl FmtConfig {
    pub fn width(&self) -> u64 {
        self.width
    }
    pub fn indent(&self) -> usize {
        self.indent
    }
    pub fn text(&self) -> String {
        self.text.join("\n")
    }
}

impl FromStr for FmtTestCommand {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let left = strip(s.trim(), "//!").and_then(|s| strip(s.trim(), "new-fmt"));
        if let Some(l) = left {
            ensure!(l.trim().is_empty(), "invalid new-fmt declaration");
        } else {
            bail!("cannot parse to fmt command, expect //! new-fmt");
        }
        Ok(Self::default())
    }
}

impl Command for FmtTestCommand {
    type ConfigEntry = ConfigEntry;

    fn has_config(&self) -> bool {
        true
    }

    fn add_config(&mut self, config: Self::ConfigEntry) -> Result<()> {
        match (self, config) {
            (FmtTestCommand::Fmt(c), ConfigEntry::Width(w)) => {
                c.width = w;
            }
            (FmtTestCommand::Fmt(c), ConfigEntry::Indent(i)) => {
                c.indent = i as usize;
            }
        }
        Ok(())
    }

    fn add_textline(&mut self, line: &str) -> Result<()> {
        match self {
            FmtTestCommand::Fmt(c) => c.text.push(line.to_string()),
        }
        Ok(())
    }

    fn validate(&self) -> Result<()> {
        Ok(())
    }
}

#[test]
fn test_config_parse() -> anyhow::Result<()> {
    let line = "//! width: 40";
    let config: ConfigEntry = line.parse()?;
    assert!(matches!(config, ConfigEntry::Width(40)));
    Ok(())
}
