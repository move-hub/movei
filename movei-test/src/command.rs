use anyhow::{Error, Result};
use std::{fmt::Debug, str::FromStr};

pub trait Command: FromStr<Err = Error> + Debug {
    type ConfigEntry: CommandConfigEntry;
    fn add_config(&mut self, config: Self::ConfigEntry) -> Result<()>;
    fn add_textline(&mut self, line: &str) -> Result<()>;
    fn validate(&self) -> Result<()>;
}

pub trait CommandConfigEntry: FromStr<Err = Error> + Debug {}
