use anyhow::{bail, Result};
use codespan::Span;
use itertools::Itertools;

pub struct Comments {
    doc_comments: Vec<Span>,
    regular_comments: Vec<Span>,
}

impl Comments {
    pub fn new(source: &str, comment_map: Vec<Span>) -> Self {
        let mut doc_comments = vec![];
        let mut regular_comments = vec![];
        for s in comment_map {
            let t = comment_type(&source[s.start().to_usize()..s.end().to_usize()])
                .expect("invalid comment span");
            match t {
                CommentType::Block | CommentType::Line => {
                    regular_comments.push(s);
                }
                CommentType::DocBlock => {
                    doc_comments.push(s);
                }
                CommentType::DocLine => {
                    doc_comments.push(s);
                }
            }
        }
        Self {
            doc_comments,
            regular_comments,
        }
    }

    // Pop comments that occur before a byte-index in the source
    pub fn pop_doc_comments(&mut self, limit: usize) -> impl Iterator<Item = Span> {
        pop_comments(&mut self.doc_comments, limit)
    }

    pub fn pop_comments(&mut self, limit: usize) -> impl Iterator<Item = Span> {
        pop_comments(&mut self.regular_comments, limit)
    }
}

fn pop_comments(c: &mut Vec<Span>, limit: usize) -> impl Iterator<Item = Span> {
    let spans = c
        .iter()
        .take_while_ref(|span| span.start().to_usize() < limit).copied()
        .collect::<Vec<_>>();
    c.drain(0..spans.len());
    spans.into_iter()
}
pub struct Comment<'a> {
    pub span: Span,
    pub content: &'a str,
}

pub enum CommentType {
    Block,
    Line,
    DocBlock,
    DocLine,
}

pub fn comment_type(content: &str) -> Result<CommentType> {
    let t = if content.starts_with("//") {
        let remaining = content.strip_prefix("//").unwrap();
        if remaining.starts_with('/') && !remaining.starts_with("//") {
            CommentType::DocLine
        } else {
            CommentType::Line
        }
    } else if content.starts_with("/*") {
        let remaining = content.strip_prefix("/*").unwrap();
        if remaining.starts_with('*') && !remaining.starts_with("**") {
            CommentType::DocBlock
        } else {
            CommentType::Block
        }
    } else {
        bail!("invalid comment content");
    };
    Ok(t)
}
