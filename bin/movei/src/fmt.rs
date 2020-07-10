use crate::FmtArgs;
use anyhow::{bail, Result};
use move_lang::{errors::FilesSourceText, parser, strip_comments_and_verify};
use movei_fmt::{format, Formatter};

pub fn run(arg: FmtArgs) -> Result<()> {
    let FmtArgs {
        width,
        indent,
        input,
        in_place,
    } = arg;

    let content = std::fs::read_to_string(input.as_path())?;
    let fname = input.as_path().display().to_string();
    let fname: &'static str = Box::leak(Box::new(fname));
    let parsed_result =
        strip_comments_and_verify(fname, content.as_str()).and_then(|(stripped, comment_map)| {
            parser::syntax::parse_file_string(fname, stripped.as_str(), Default::default())
                .map(|(d, _c)| (d, comment_map))
        });
    // TODO: strip comment
    match parsed_result {
        Ok((defs, comments)) => {
            match defs.first() {
                None => bail!("source code has no definitions"),
                Some(def) => {
                    let formatter = Formatter::new(content.as_str(), comments, indent);
                    let doc = formatter.definition(def);
                    let output = format(width as isize, doc);
                    if in_place {
                        std::fs::write(input.as_path(), output)?;
                    } else {
                        println!("{}", output);
                    }
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
