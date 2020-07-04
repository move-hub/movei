use crate::{context::MoveiContext, FmtArgs};
use anyhow::{bail, Result};
use move_lang::{errors::FilesSourceText, parser};
use movei_fmt::{pretty, Formatter};
use std::collections::BTreeMap;

pub fn run(arg: FmtArgs) -> Result<()> {
    let FmtArgs { width, input } = arg;

    let content = std::fs::read_to_string(input.as_path())?;
    let fname = input.as_path().display().to_string();
    let fname: &'static str = Box::leak(Box::new(fname));

    // TODO: strip comment
    match parser::syntax::parse_file_string(fname, content.as_str(), BTreeMap::new()) {
        Ok((defs, _comments)) => {
            match defs.first() {
                None => bail!("source code has no definitions"),
                Some(def) => {
                    let doc = Formatter::definition(def);
                    let output = pretty::format(width as isize, doc);
                    println!("{}", output);
                }
            };
            Ok(())
        }
        Err(errs) => {
            let mut files = FilesSourceText::new();
            files.insert(fname, content);
            let error_buffer = move_lang::errors::report_errors_to_color_buffer(files, errs);
            println!("{}", String::from_utf8_lossy(error_buffer.as_slice()));
            Ok(())
        }
    }
}
