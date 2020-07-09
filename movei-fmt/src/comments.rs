use anyhow::{bail, Result};
use codespan::Span;

use std::convert::TryFrom;

pub struct Comments {
    comments: Vec<Span>,
}

impl Comments {
    pub fn new(comment_map: Vec<Span>) -> Self {
        Self {
            comments: comment_map,
        }
    }

    pub fn pop_comments_between(&mut self, span: Span) -> impl Iterator<Item = Span> {
        let mut begin = None;
        let mut end = None;

        for (i, s) in self.comments.iter().enumerate() {
            if s.start() >= span.start() && s.end() <= span.end() {
                if begin.is_none() {
                    begin = Some(i);
                }
            } else if begin.is_some() {
                end = Some(i);
                break;
            }
        }
        let total_len = self.comments.len();
        let drain_range = match (begin, end) {
            (Some(b), None) => (b..total_len),
            (Some(b), Some(e)) => (b..e),
            (None, _) => return vec![].into_iter(),
        };
        self.comments
            .drain(drain_range)
            .collect::<Vec<_>>()
            .into_iter()
    }
}

pub struct Comment<'a> {
    pub span: Span,
    pub content: &'a str,
    pub ty: CommentType,
}

impl<'s> TryFrom<(Span, &'s str)> for Comment<'s> {
    type Error = anyhow::Error;

    fn try_from(d: (Span, &'s str)) -> Result<Self, Self::Error> {
        let (span, source) = d;
        let content = &source[span.start().to_usize()..span.end().to_usize()];
        comment_type(content).map(|ty| Comment { span, content, ty })
    }
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
