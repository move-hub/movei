use anyhow::{Error, Result};

use std::{fmt::Debug, str::FromStr};

pub use functional_tests::evaluator::{EvaluationLog, EvaluationOutput, Status};

pub trait Command: FromStr<Err = Error> + Debug + Default {
    type ConfigEntry: CommandConfigEntry;
    fn add_config(&mut self, config: Self::ConfigEntry) -> Result<()>;
    fn add_textline(&mut self, line: &str) -> Result<()>;
    fn validate(&self) -> Result<()>;
}

pub trait CommandConfigEntry: FromStr<Err = Error> + Debug {}

pub trait CommandEvaluator {
    type Cmd: Command;
    fn eval(&self, commands: Vec<Self::Cmd>) -> Result<EvaluationLog>;
}
