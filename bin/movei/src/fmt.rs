use crate::{package::Package, utils::get_package_root, FmtArgs};
use anyhow::{bail, Result};
use itertools::Itertools;
use move_lang::{errors::FilesSourceText, parser, strip_comments_and_verify, MOVE_EXTENSION};
use movei_fmt::{format, Formatter};
use std::path::{Path, PathBuf};

pub fn run(arg: FmtArgs) -> Result<()> {
    let FmtArgs {
        width,
        indent,
        input,
        in_place,
    } = arg;

    let package = get_package_root()?.map(|p| Package::load(p)).transpose()?;
    let lookup_paths = input
        .map(|p| vec![p])
        .or_else(|| package.map(|p| vec![p.module_dir(), p.script_dir()]))
        .unwrap_or_default();
    let move_files: Vec<_> = lookup_paths
        .into_iter()
        .flat_map(|p| find_move_file(p).into_iter())
        .collect();
    for f in move_files {
        if let Err(e) = format_file(f.as_path(), !in_place, width, indent) {
            eprintln!("Skip format {}, reason: {:?}", f.display(), e);
        }
    }
    Ok(())
}

pub fn format_file<P: AsRef<Path>>(
    input: P,
    dry_run: bool,
    width: u32,
    indent: usize,
) -> Result<()> {
    let content = std::fs::read_to_string(input.as_ref())?;

    let fname = input.as_ref().display().to_string();
    let fname: &'static str = Box::leak(Box::new(fname));
    let parsed_result = strip_comments_and_verify(fname, content.as_str()).and_then(
        |(stripped, mut comment_map, mut regular_comments)| {
            parser::syntax::parse_file_string(fname, stripped.as_str(), comment_map.clone()).map(
                |(d, _c)| {
                    comment_map.append(&mut regular_comments);
                    (d, comment_map)
                },
            )
        },
    );
    // TODO: strip comment
    match parsed_result {
        Ok((defs, comments)) => {
            match defs.first() {
                None => bail!("source code has no definitions"),
                Some(def) => {
                    let formatter = Formatter::new(content.as_str(), comments, indent);
                    let doc = formatter.definition(def);
                    let output = format(width as isize, doc);
                    let output = {
                        // trim empty lines
                        output.lines().map(|l| l.trim_end()).join("\n")
                    };
                    if !dry_run {
                        std::fs::write(input.as_ref(), output)?;
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
            let err =
                anyhow::Error::msg(String::from_utf8_lossy(error_buffer.as_slice()).to_string());
            Err(err)
        }
    }
}

pub fn find_move_file(path: PathBuf) -> Vec<PathBuf> {
    let has_move_extension = |path: &Path| match path.extension().and_then(|s| s.to_str()) {
        Some(extension) => extension == MOVE_EXTENSION,
        None => false,
    };

    let mut result = vec![];

    if !path.exists() {
        return result;
    }

    if !path.is_dir() {
        // If the filename is specified directly, add it to the list, regardless
        // of whether it has a ".move" extension.
        result.push(path);
    } else {
        for entry in walkdir::WalkDir::new(path)
            .into_iter()
            .filter_map(|e| e.ok())
        {
            let entry_path = entry.path();

            if !entry.file_type().is_file() || !has_move_extension(&entry_path) {
                continue;
            }
            result.push(entry.into_path());
        }
    }
    result
}
