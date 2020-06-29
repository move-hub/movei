pub mod command_parser;
pub use functional_tests::evaluator::{
    EvaluationLog, EvaluationOutput, OutputType, Stage, Status, TransactionId,
};

use anyhow::{Error, Result};
use functional_tests::{checker::match_output, testsuite::println_match_result};
use std::{fmt::Debug, fs::read_to_string, path::Path, str::FromStr};

pub trait Command: FromStr<Err = Error> + Debug + Default {
    type ConfigEntry: CommandConfigEntry;
    fn has_config(&self) -> bool;
    fn add_config(&mut self, config: Self::ConfigEntry) -> Result<()>;
    fn add_textline(&mut self, line: &str) -> Result<()>;
    fn validate(&self) -> Result<()>;
}

pub trait CommandConfigEntry: FromStr<Err = Error> + Debug {}

pub trait CommandEvaluator {
    type Cmd: Command;
    fn eval(&self, commands: Vec<Self::Cmd>) -> Result<EvaluationLog>;
}

pub fn functional_tests<P: Command, Evaluator: CommandEvaluator<Cmd = P>>(
    evaluator: Evaluator,
    path: &Path,
) -> datatest::Result<bool> {
    let input = read_to_string(path)?;
    let lines: Vec<String> = input.lines().map(|line| line.to_string()).collect();
    let (commands, check_directives): (Vec<P>, _) = command_parser::parse(&lines)?;
    let logs = evaluator.eval(commands)?;
    let match_result = match_output(&logs, &check_directives);
    if match_result.is_success() {
        return Ok(true);
    }
    println_match_result(
        path,
        lines.as_slice(),
        check_directives,
        match_result,
        &logs,
    )?;
    Ok(false)
}
