mod command;

use anyhow::Result;
use difference::assert_diff;
use move_lang::parser;
use movei_fmt::{pretty, Formatter};
use movei_test::command_parser;
use std::path::Path;

pub fn functional_test(p: &Path) -> datatest::Result<()> {
    let content = std::fs::read_to_string(p)?;

    let lines = content.lines().peekable();
    let (commands, _checks) = command_parser::parse(lines)?;
    for cmd in commands {
        match cmd {
            command::FmtTestCommand::Fmt(c) => {
                run_fmt(c.width(), c.indent(), c.text().as_str())?;
            }
        }
    }
    Ok(())
}

fn run_fmt(width: u64, indent: usize, text: &str) -> Result<()> {
    let text = text.trim();
    let (stripped, comments) = move_lang::strip_comments_and_verify("test", text).unwrap();
    let (defs, _comments) =
        parser::syntax::parse_file_string("test", stripped.as_str(), comments.clone()).unwrap();
    let def = defs.first().unwrap();
    let formatter = Formatter::new(text, comments, indent);
    let doc = formatter.definition(def);
    let res = pretty::format(width as isize, doc);
    assert_diff!(text, res.as_str(), "\n", 0);
    Ok(())
}
