//!  This module implements the functionality described in
//!  ["Strictly Pretty" (2000) by Christian Lindig][0], with a few
//!  extensions.
//!
//!  [0]: http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.34.2200

/// The implementation is Copied from gleam-lang source repo.
/// All right reserved to gleam-lang team.
use im::vector::Vector;
use itertools::Itertools;

pub trait Documentable {
    fn to_doc(&self) -> Document;
}

impl<'r, T: Documentable> Documentable for &'r T {
    fn to_doc(&self) -> Document {
        (*self).to_doc()
    }
}

impl Documentable for &str {
    fn to_doc(&self) -> Document {
        Document::Text(self.to_string())
    }
}

impl Documentable for String {
    fn to_doc(&self) -> Document {
        Document::Text(self.clone())
    }
}

impl Documentable for isize {
    fn to_doc(&self) -> Document {
        Document::Text(format!("{}", self))
    }
}

impl Documentable for i64 {
    fn to_doc(&self) -> Document {
        Document::Text(format!("{}", self))
    }
}

impl Documentable for usize {
    fn to_doc(&self) -> Document {
        Document::Text(format!("{}", self))
    }
}

impl Documentable for f64 {
    fn to_doc(&self) -> Document {
        Document::Text(format!("{:?}", self))
    }
}

impl Documentable for u64 {
    fn to_doc(&self) -> Document {
        Document::Text(format!("{:?}", self))
    }
}
impl Documentable for u128 {
    fn to_doc(&self) -> Document {
        Document::Text(format!("{:?}", self))
    }
}

impl Documentable for Document {
    fn to_doc(&self) -> Document {
        self.clone()
    }
}

impl Documentable for Vec<Document> {
    fn to_doc(&self) -> Document {
        concat(self.clone().into_iter())
    }
}

impl<D: Documentable> Documentable for Option<D> {
    fn to_doc(&self) -> Document {
        match self {
            Some(d) => d.to_doc(),
            None => Document::Nil,
        }
    }
}

pub fn concat(mut docs: impl Iterator<Item = Document>) -> Document {
    let init = docs.next().unwrap_or_else(|| nil());
    docs.fold(init, |acc, doc| acc.append(doc))
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Document {
    /// Returns a document entity used to represent nothingness
    Nil,

    /// A mandatory linebreak
    Line(usize),

    /// Forces contained groups to break
    ForceBreak,

    /// May break contained document based on best fit, thus flex break
    FlexBreak(String, Box<Document>),

    /// Renders `broken` if group is broken, `unbroken` otherwise
    Break {
        broken: String,
        unbroken: String,
    },

    /// Join 2 documents together
    Cons(Box<Document>, Box<Document>),

    /// Nests the given document by the given indent
    Nest(isize, Box<Document>),

    /// Nests the given document to the current cursor position
    NestCurrent(Box<Document>),

    /// Nests the given document to the current cursor position
    Group(String, Box<Document>),

    // May nest the given document based on best fit, thus flex group
    FlexGroup(String, isize, Box<Document>),

    /// A string to render
    Text(String),
}

#[derive(Debug, Clone)]
enum Mode {
    Broken,
    Unbroken,
}

fn fits(mut limit: isize, mut docs: Vector<(isize, Mode, Document)>) -> bool {
    loop {
        if limit < 0 {
            return false;
        };

        let (indent, mode, document) = match docs.pop_front() {
            Some(x) => x,
            None => return true,
        };

        match document {
            Document::Nil => (),

            Document::Line(_) => return true,

            Document::ForceBreak => return false,

            Document::FlexGroup(_, i, doc) => docs.push_front((i + indent, mode, *doc)),

            Document::Nest(i, doc) => docs.push_front((i + indent, mode, *doc)),

            Document::NestCurrent(doc) => docs.push_front((indent, mode, *doc)),

            Document::Group(_, doc) => docs.push_front((indent, Mode::Unbroken, *doc)),

            Document::Text(s) => limit -= s.len() as isize,

            Document::Break { unbroken, .. } => match mode {
                Mode::Broken => return true,
                Mode::Unbroken => limit -= unbroken.len() as isize,
            },

            Document::FlexBreak(_, doc) => docs.push_front((indent, mode, *doc)),

            Document::Cons(left, right) => {
                docs.push_front((indent, mode.clone(), *right));
                docs.push_front((indent, mode, *left));
            }
        }
    }
}

pub fn format(limit: isize, doc: Document) -> String {
    let mut buffer = String::new();
    fmt(
        &mut buffer,
        limit,
        0,
        vector![(
            0,
            Mode::Unbroken,
            Document::Group("global".to_string(), Box::new(doc))
        )],
    );
    // TODO: figure out more performance way
    // trim empty lines
    buffer.lines().map(|l| l.trim_end()).join("\n")
}

fn fmt(b: &mut String, limit: isize, mut width: isize, mut docs: Vector<(isize, Mode, Document)>) {
    while let Some((indent, mode, document)) = docs.pop_front() {
        match document {
            Document::Nil | Document::ForceBreak => (),

            Document::Line(i) => {
                for _ in 0..i {
                    b.push_str("\n");
                }
                b.push_str(" ".repeat(indent as usize).as_str());
                width = indent;
            }

            Document::Break { broken, unbroken } => {
                width = match mode {
                    Mode::Unbroken => {
                        b.push_str(unbroken.as_str());
                        width + unbroken.len() as isize
                    }
                    Mode::Broken => {
                        b.push_str(broken.as_str());
                        b.push_str("\n");
                        b.push_str(" ".repeat(indent as usize).as_str());
                        indent as isize
                    }
                };
            }

            Document::Text(s) => {
                width += s.len() as isize;
                b.push_str(s.as_str());
            }

            Document::Cons(left, right) => {
                docs.push_front((indent, mode.clone(), *right));
                docs.push_front((indent, mode, *left));
            }

            Document::Nest(i, doc) => {
                docs.push_front((indent + i, mode, *doc));
            }

            Document::NestCurrent(doc) => {
                docs.push_front((width, mode, *doc));
            }

            Document::Group(label, doc) => {
                docs.push_front((indent, Mode::Unbroken, (*doc).clone()));
                let fitted = fits(limit - width, docs.clone());
                trace!(
                    "{}group: {}, indent: {:?}, limit: {}, fit: {}",
                    " ".repeat(indent as usize),
                    &label,
                    indent,
                    limit - width,
                    fitted
                );
                if !fitted {
                    docs.pop_front();
                    docs.push_front((indent, Mode::Broken, (*doc).clone()));
                }
            }

            Document::FlexBreak(label, doc) => {
                docs.push_front((indent, Mode::Unbroken, (*doc).clone()));
                let fitted = fits(limit - width, docs.clone());
                trace!(
                    "{}flexbreak: {}, indent: {:?}, limit: {}, fit: {}",
                    " ".repeat(indent as usize),
                    &label,
                    indent,
                    limit - width,
                    fitted
                );
                if !fitted {
                    docs.pop_front();
                    docs.push_front((indent, Mode::Broken, (*doc).clone()));
                }
            }

            Document::FlexGroup(label, i, doc) => {
                docs.push_front((indent, Mode::Unbroken, (*doc).clone()));
                let fitted = fits(limit - width, docs.clone());
                trace!(
                    "{}flexgroup: {}, indent: {:?}, limit: {}, fit: {}",
                    " ".repeat(indent as usize),
                    &label,
                    indent,
                    limit - width,
                    fitted
                );
                if !fitted {
                    docs.pop_front();
                    docs.push_front((indent, Mode::Broken, line()));
                    docs.push_front((indent + i, Mode::Broken, line().append((*doc).clone())));
                }
            }
        }
    }
}

#[inline]
pub fn nil() -> Document {
    Document::Nil
}

#[inline]
pub fn space() -> Document {
    " ".to_doc()
}
#[inline]
pub fn line() -> Document {
    Document::Line(1)
}

#[inline]
pub fn lines(i: usize) -> Document {
    Document::Line(i)
}

#[inline]
pub fn force_break() -> Document {
    Document::ForceBreak
}

#[inline]
pub fn break_(broken: &str, unbroken: &str) -> Document {
    Document::Break {
        broken: broken.to_string(),
        unbroken: unbroken.to_string(),
    }
}

#[inline]
pub fn delim(d: &str) -> Document {
    Document::Break {
        broken: d.to_string(),
        unbroken: format!("{} ", d),
    }
}

#[inline]
pub fn flex_group(label: String, ident: isize, d: impl Documentable) -> Document {
    Document::FlexGroup(label, ident, Box::new(d.to_doc()))
}

#[inline]
pub fn group(label: String, d: impl Documentable) -> Document {
    Document::Group(label, Box::new(d.to_doc()))
}

#[inline]
pub fn nest(indent: isize, d: impl Documentable) -> Document {
    d.to_doc().nest(indent)
}

impl Document {
    #[inline]
    pub fn group(self, label: String) -> Document {
        Document::Group(label, Box::new(self))
    }

    #[inline]
    pub fn flex_group(self, label: String, indent: isize) -> Document {
        Document::FlexGroup(label, indent, Box::new(self))
    }

    #[inline]
    pub fn flex_break(self, label: String) -> Document {
        Document::FlexBreak(label, Box::new(self))
    }

    #[inline]
    pub fn nest(self, indent: isize) -> Document {
        Document::Nest(indent, Box::new(self))
    }

    #[inline]
    pub fn nest_current(self) -> Document {
        Document::NestCurrent(Box::new(self))
    }

    #[inline]
    pub fn breakable_append(self, x: impl Documentable) -> Document {
        self.append(break_("", "")).append(x)
    }
    #[inline]
    pub fn append(self, x: impl Documentable) -> Document {
        let x = x.to_doc();
        if matches!(x, Document::Nil) {
            self
        } else if matches!(self, Document::Nil) {
            x
        } else {
            Document::Cons(Box::new(self), Box::new(x))
        }
    }

    pub fn format(self, limit: isize) -> String {
        format(limit, self)
    }

    #[inline]
    pub fn surround(self, open: impl Documentable, closed: impl Documentable) -> Document {
        open.to_doc().append(self).append(closed)
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn fits_test() {
        use super::{Document::*, Mode::*, *};

        // Negative limits never fit
        assert!(!fits(-1, vector![]));

        // If no more documents it always fits
        assert!(fits(0, vector![]));

        // ForceBreak never fits
        assert!(!fits(100, vector![(0, Unbroken, ForceBreak)]));
        assert!(!fits(100, vector![(0, Broken, ForceBreak)]));

        // Break in Broken fits always
        assert!(fits(
            1,
            vector![(
                0,
                Broken,
                Break {
                    broken: "12".to_string(),
                    unbroken: "".to_string()
                }
            )]
        ));

        // Break in Unbroken mode fits if `unbroken` fits
        assert!(fits(
            3,
            vector![(
                0,
                Unbroken,
                Break {
                    broken: "".to_string(),
                    unbroken: "123".to_string()
                }
            )]
        ));
        assert!(!fits(
            2,
            vector![(
                0,
                Unbroken,
                Break {
                    broken: "".to_string(),
                    unbroken: "123".to_string()
                }
            )]
        ));

        // Line always fits
        assert!(fits(0, vector![(0, Broken, Line(100))]));
        assert!(fits(0, vector![(0, Unbroken, Line(100))]));

        // String fits if smaller than limit
        assert!(fits(5, vector![(0, Broken, Text("Hello".to_string()))]));
        assert!(fits(5, vector![(0, Unbroken, Text("Hello".to_string()))]));
        assert!(!fits(4, vector![(0, Broken, Text("Hello".to_string()))]));
        assert!(!fits(4, vector![(0, Unbroken, Text("Hello".to_string()))]));

        // Cons fits if combined smaller than limit
        assert!(fits(
            2,
            vector![(
                0,
                Broken,
                Cons(
                    Box::new(Text("1".to_string())),
                    Box::new(Text("2".to_string()))
                )
            )]
        ));
        assert!(fits(
            2,
            vector![(
                0,
                Unbroken,
                Cons(
                    Box::new(Text("1".to_string())),
                    Box::new(Text("2".to_string()))
                )
            )]
        ));
        assert!(!fits(
            1,
            vector![(
                0,
                Broken,
                Cons(
                    Box::new(Text("1".to_string())),
                    Box::new(Text("2".to_string()))
                )
            )]
        ));
        assert!(!fits(
            1,
            vector![(
                0,
                Unbroken,
                Cons(
                    Box::new(Text("1".to_string())),
                    Box::new(Text("2".to_string()))
                )
            )]
        ));

        // Nest fits if combined smaller than limit
        assert!(fits(
            2,
            vector![(0, Broken, Nest(1, Box::new(Text("12".to_string())),))]
        ));
        assert!(fits(
            2,
            vector![(0, Unbroken, Nest(1, Box::new(Text("12".to_string())),))]
        ));
        assert!(!fits(
            1,
            vector![(0, Broken, Nest(1, Box::new(Text("12".to_string())),))]
        ));
        assert!(!fits(
            1,
            vector![(0, Unbroken, Nest(1, Box::new(Text("12".to_string()))))]
        ));

        // Nest fits if combined smaller than limit
        assert!(fits(
            2,
            vector![(0, Broken, NestCurrent(Box::new(Text("12".to_string())),))]
        ));
        assert!(fits(
            2,
            vector![(0, Unbroken, NestCurrent(Box::new(Text("12".to_string())),))]
        ));
        assert!(!fits(
            1,
            vector![(0, Broken, NestCurrent(Box::new(Text("12".to_string())),))]
        ));
        assert!(!fits(
            1,
            vector![(0, Unbroken, NestCurrent(Box::new(Text("12".to_string()))))]
        ));
    }

    #[test]
    fn format_test() {
        use super::{Document::*, *};

        let doc = Text("Hi".to_string());
        assert_eq!("Hi".to_string(), format(10, doc));

        let doc = Cons(
            Box::new(Text("Hi".to_string())),
            Box::new(Text(", world!".to_string())),
        );
        assert_eq!("Hi, world!".to_string(), format(10, doc));

        let doc = Nil;
        assert_eq!("".to_string(), format(10, doc));

        let doc = Break {
            broken: "broken".to_string(),
            unbroken: "unbroken".to_string(),
        };
        assert_eq!("unbroken".to_string(), format(10, doc));

        let doc = Break {
            broken: "broken".to_string(),
            unbroken: "unbroken".to_string(),
        };
        assert_eq!("broken\n".to_string(), format(5, doc));

        let doc = Nest(
            2,
            Box::new(Cons(
                Box::new(Text("1".to_string())),
                Box::new(Cons(Box::new(Line(1)), Box::new(Text("2".to_string())))),
            )),
        );
        assert_eq!("1\n  2".to_string(), format(1, doc));

        let doc = Cons(
            Box::new(Text("111".to_string())),
            Box::new(NestCurrent(Box::new(Cons(
                Box::new(Line(1)),
                Box::new(Text("2".to_string())),
            )))),
        );
        assert_eq!("111\n   2".to_string(), format(1, doc));

        let doc = Cons(
            Box::new(ForceBreak),
            Box::new(Break {
                broken: "broken".to_string(),
                unbroken: "unbroken".to_string(),
            }),
        );
        assert_eq!("broken\n".to_string(), format(100, doc));
    }
}
