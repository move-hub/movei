#![feature(move_ref_pattern)]
#![feature(drain_filter)]

pub mod command;
pub mod command_impl;
pub mod command_parser;
pub mod move_test_compiler;

use command::*;
use functional_tests::{checker::match_output, testsuite::println_match_result};
use std::{fs::read_to_string, path::Path};

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
