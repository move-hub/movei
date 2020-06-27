use anyhow::{format_err, Error, Result};
use std::{fmt::Debug, fs::read_to_string, path::Path, str::FromStr};

pub trait Command: FromStr<Err = Error> + Debug {
    type ConfigEntry: CommandConfigEntry;
    fn add_config(&mut self, config: Self::ConfigEntry) -> Result<()>;
    fn add_textline(&mut self, line: &str) -> Result<()>;
    fn validate(&self) -> Result<()>;
}

pub trait CommandConfigEntry: FromStr<Err = Error> + Debug {}

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

pub fn functional_tests<P: Command>(path: &Path) -> datatest::Result<()> {
    let input = read_to_string(path)?;
    let mut commands = vec![];
    let mut cur_command: Option<P> = None;
    for (_line_idx, line) in input.lines().enumerate() {
        let line = line.trim();
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
                        Err(format_err!("invalid config directive, {:?}", c))?;
                    }
                }
            },
            Err(_) => {
                if let Some(c) = cur_command.as_mut() {
                    c.add_textline(line)?;
                } else {
                    // skip lines with unknown contexts.
                    continue;
                }
            }
        }
    }
    todo!()
}
