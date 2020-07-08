use colored::Colorize;
use std::fmt::Display;

pub fn color_diff<'a>(old: &'a str, new: &'a str) -> ColorDiff<'a> {
    let diff_result = diff::lines(old, new);
    ColorDiff { diff: diff_result }
}

#[derive(Clone, Debug)]
pub struct ColorDiff<'a> {
    diff: Vec<diff::Result<&'a str>>,
}

impl<'a> ColorDiff<'a> {
    pub fn difference(&self) -> usize {
        self.diff
            .iter()
            .filter(|d| !matches!(d, diff::Result::Both(_, _)))
            .count()
    }
}

impl<'a> Display for ColorDiff<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for d in &self.diff {
            match d {
                diff::Result::Both(co, _) => {
                    writeln!(f, "{}", co)?;
                }
                diff::Result::Right(x) => {
                    if x.trim().is_empty() {
                        writeln!(f, "{}", " ".on_green())?;
                    } else {
                        writeln!(f, "{}", x.green())?;
                    }
                }
                diff::Result::Left(x) => {
                    if x.trim().is_empty() {
                        writeln!(f, "{}", " ".on_red())?;
                    } else {
                        writeln!(f, "{}", x.red())?;
                    }
                }
            }
        }

        Ok(())
    }
}
