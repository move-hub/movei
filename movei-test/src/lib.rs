#![feature(move_ref_pattern)]

pub mod command;
pub mod command_impl;

use anyhow::{bail, format_err, Error, Result};
use functional_tests::{checker, common::LineSp};
use std::{fmt::Debug, fs::read_to_string, path::Path, str::FromStr};

use command::*;
use functional_tests::{checker::match_output, testsuite::println_match_result};

pub type CheckerDirective = checker::Directive;
pub type LineSpCheckerDirective = LineSp<CheckerDirective>;

pub enum Directive<C>
where
    C: Command,
{
    CommandDirective(C),
    CommandConfigDirective(C::ConfigEntry),
}

impl<C> FromStr for Directive<C>
where
    C: Command,
{
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parsed = s
            .parse::<C>()
            .map(|s| Directive::CommandDirective(s))
            .or_else(|_| {
                s.parse::<C::ConfigEntry>()
                    .map(|i| Directive::CommandConfigDirective(i))
            });
        parsed.map_err(|_e| format_err!("cannot parse into directive"))
    }
}

pub fn extract_global_config<T: FromStr>(
    lines: impl IntoIterator<Item = impl AsRef<str>>,
) -> Vec<T> {
    let mut entries = vec![];

    for line in lines.into_iter() {
        let line = line.as_ref();

        if let Ok(entry) = line.parse::<T>() {
            entries.push(entry);
            continue;
        }
    }

    entries
}

pub fn parse<P: Command>(
    lines: impl IntoIterator<Item = impl AsRef<str>>,
) -> Result<(Vec<P>, Vec<LineSpCheckerDirective>)> {
    let mut commands = vec![];
    let mut check_directives: Vec<LineSpCheckerDirective> = vec![];
    let mut cur_command: Option<P> = None;
    for (line_idx, line) in lines.into_iter().enumerate() {
        let line = line.as_ref().trim();
        // skip empty lines
        if line.is_empty() {
            continue;
        }
        match line.parse::<Directive<P>>() {
            Ok(directive) => match directive {
                Directive::CommandDirective(c) => {
                    if let Some(c) = cur_command.take() {
                        c.validate()?;
                        commands.push(c);
                    }
                    cur_command = Some(c);
                }
                Directive::CommandConfigDirective(c) => {
                    if let Some(command) = cur_command.as_mut() {
                        command.add_config(c)?;
                    } else {
                        bail!("invalid config directive, {:?}", c);
                    }
                }
            },
            Err(_) => {
                if let Ok(directives) = checker::Directive::parse_line(line) {
                    check_directives
                        .extend(directives.into_iter().map(|sp| sp.into_line_sp(line_idx)));
                } else if let Some(c) = cur_command.as_mut() {
                    c.add_textline(line)?;
                } else {
                    // skip lines with unknown contexts.
                    continue;
                }
            }
        }
    }
    if let Some(command) = cur_command {
        command.validate()?;
        commands.push(command);
    }
    Ok((commands, check_directives))
}

pub fn functional_tests<P: Command, Evaluator: CommandEvaluator<Cmd = P>>(
    evaluator: Evaluator,
    path: &Path,
) -> datatest::Result<bool> {
    let input = read_to_string(path)?;
    let lines: Vec<String> = input.lines().map(|line| line.to_string()).collect();
    let (commands, check_directives): (Vec<P>, _) = parse(&lines)?;
    let mut logs = vec![];
    for c in commands {
        let l = evaluator.eval(c)?;
        logs.push(l);
    }

    let outputs: Vec<_> = logs.into_iter().flat_map(|l| l.outputs).collect();
    let logs = EvaluationLog { outputs };
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
