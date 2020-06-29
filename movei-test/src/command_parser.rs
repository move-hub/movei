use anyhow::{bail, format_err, Error, Result};
use functional_tests::{checker, common::LineSp};
use std::str::FromStr;

use crate::command::Command;

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
            .map(Directive::CommandDirective)
            .or_else(|_| {
                s.parse::<C::ConfigEntry>()
                    .map(Directive::CommandConfigDirective)
            });
        parsed.map_err(|_e| format_err!("cannot parse into directive"))
    }
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
                    // init default command context
                    let mut default_command = P::default();
                    default_command.add_textline(line)?;
                    cur_command = Some(default_command);
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
