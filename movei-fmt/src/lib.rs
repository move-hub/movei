#[macro_use]
extern crate im;
pub mod pretty;

use crate::pretty::{break_, concat, delim, group, line, lines, nest, nil, Document, Documentable};
use codespan::Span;
use itertools::Itertools;
use move_ir_types::location::Spanned;
use move_lang::{
    parser::{ast, ast::Type_},
    shared::Name,
    FileCommentMap,
};
use std::{borrow::BorrowMut, cell::RefCell, collections::BTreeMap, rc::Rc};

struct Comment<'a> {
    pub span: Span,
    pub content: &'a str,
}
pub enum CommentType {
    Block,
    Line,
    // Unknown
}

impl<'a> Comment<'a> {
    pub fn comment_type(&self) -> CommentType {
        if self.content.starts_with("//") {
            return CommentType::Line;
        } else if self.content.starts_with("/*") {
            return CommentType::Block;
        } else {
            unreachable!()
        }
    }
}

pub struct Formatter<'a> {
    inner: RefCell<Inner<'a>>,
}

impl<'a> Formatter<'a> {
    pub fn new(source: &'a str, comment_map: FileCommentMap) -> Self {
        let inner = Inner::new(source, comment_map);

        Self {
            inner: RefCell::new(inner),
        }
    }

    pub fn pop_doc_comments(&self, limit: usize) -> impl Iterator<Item = Comment<'a>> {
        self.inner.borrow_mut().pop_doc_comments(limit)
    }
}

struct Inner<'a> {
    doc_comments: Vec<Span>,
    source: &'a str,
}

impl<'a> Inner<'a> {
    pub fn new(source: &'a str, comment_map: FileCommentMap) -> Self {
        Self {
            source,
            doc_comments: comment_map.keys().cloned().collect(),
        }
    }
    // Pop comments that occur before a byte-index in the source
    pub fn pop_doc_comments(&mut self, limit: usize) -> impl Iterator<Item = Comment<'a>> {
        let spans = self
            .doc_comments
            .iter()
            .take_while_ref(|span| span.start().to_usize() < limit)
            .map(|s| *s)
            .collect::<Vec<_>>();
        self.doc_comments.drain(0..spans.len());
        let mut comments = Vec::with_capacity(spans.len());
        for s in spans {
            let comment = Comment {
                span: s,
                content: &self.source[s.start().to_usize()..s.end().to_usize()],
            };
            comments.push(comment);
        }
        comments.into_iter()
    }
}

const INDENT: isize = 2isize;

macro_rules! indent {
    ($n:literal, $doc:expr) => {
        pretty::line()
            .append(Document::ForceBreak)
            .append($doc)
            .nest($n)
    };
}
//
// macro_rules! nest {
//     ($n:literal, $($doc:expr),*) => {{
//         Document::Nil$(
//             .append($doc)
//         )*.nest($n)
//     }};
// }

macro_rules! group {
    ($($doc:expr),+) => {
        Document::Nil$(.append($doc))+.group()
    };
}

macro_rules! concats {
    ($($doc:expr),*) => {{
        Document::Nil$(
            .append($doc)
        )*
    }};
}

impl<'a> Formatter<'a> {
    pub fn definition(&mut self, expr: &ast::Definition) -> Document {
        match expr {
            ast::Definition::Script(s) => self.script(s),
            _ => todo!(),
        }
    }

    pub fn script(&mut self, script: &ast::Script) -> Document {
        let ast::Script {
            loc: loc,
            uses,
            constants,
            function,
            specs: _,
        } = script;
        let comments = comments(self.pop_doc_comments(loc.span().start().to_usize()));

        let uses = uses.to_doc();
        let consts = concat(
            constants
                .iter()
                .map(|c| self.constant_(c))
                .intersperse(line()),
        );
        let func = self.fn_(function);
        let body = {
            let userful_items = vec![uses, consts, func]
                .into_iter()
                .filter(|d| !matches!(d, Document::Nil));
            concat(userful_items.intersperse(lines(2)))
        };
        let body = "script "
            .to_doc()
            .append("{")
            .append(indent!(2, body))
            .append(line())
            .append("}");

        comments.append(body)
    }

    fn fn_(&mut self, function: &ast::Function) -> Document {
        let comments = comments(self.pop_doc_comments(function.loc.span().start().to_usize()));
        comments.append(function.to_doc())
    }

    fn constant_(&mut self, constant: &ast::Constant) -> Document {
        let loc = constant.loc;
        let comments = comments(self.pop_doc_comments(loc.span().start().to_usize()));
        comments.append(constant)
    }
}

fn comments<'a>(items: impl Iterator<Item = Comment<'a>>) -> Document {
    let mut items = items.peekable();

    if items.peek().is_none() {
        return nil();
    }
    concat(items.map(|i| i.content.to_doc()).intersperse(line())).append(line())
}

pub fn wrap_args<I>(open: &str, close: &str, delim: &str, args: I) -> Document
where
    I: Iterator<Item = Document>,
{
    let mut args = args.peekable();
    if let None = args.peek() {
        return format!("{}{}", open, close).to_doc();
    }
    // open.to_doc()
    //     .append(concat(args.intersperse(pretty::delim(delim))).flex_group(INDENT))
    //     .append(close.to_doc())
    break_(open, open)
        .append(concat(args.intersperse(pretty::delim(delim))))
        .nest(INDENT)
        .append(break_(delim, ""))
        .append(close)
        .group()
}

pub fn wrap_list<I, D: Documentable>(open: &str, close: &str, delim: &str, args: I) -> Document
where
    I: Iterator<Item = D>,
{
    let mut args = args.peekable();
    if let None = args.peek() {
        return format!("{}{}", open, close).to_doc();
    }
    let items = args.map(|d| d.to_doc()).intersperse(pretty::delim(delim));
    break_(open, open)
        .append(concat(items))
        .nest(INDENT)
        .append(break_(delim, ""))
        .append(close)
}

impl Documentable for ast::Function {
    fn to_doc(&self) -> Document {
        let ast::Function {
            visibility,
            signature,
            acquires,
            name,
            body,
            ..
        } = self;
        let native_opt = if let ast::FunctionBody_::Native = &body.value {
            "native ".to_doc()
        } else {
            nil()
        };

        concats!(visibility, native_opt, "fun ", name, signature, acquires, " ", body)
    }
}

type AcquireList = Vec<ast::ModuleAccess>;
impl Documentable for AcquireList {
    fn to_doc(&self) -> Document {
        if self.is_empty() {
            return nil();
        };
        let acquires = wrap_args("", "", ",", self.iter().map(|ma| ma.to_doc()));
        "acquires ".to_doc().append(acquires)
    }
}

impl Documentable for ast::FunctionBody_ {
    fn to_doc(&self) -> Document {
        use ast::FunctionBody_ as B;
        match self {
            B::Native => ";".to_doc(),
            B::Defined(s) => s.to_doc(),
        }
    }
}

impl Documentable for ast::FunctionVisibility {
    fn to_doc(&self) -> Document {
        use ast::FunctionVisibility as F;
        match self {
            F::Internal => nil(),
            F::Public(_) => "public ".to_doc(),
        }
    }
}
impl Documentable for ast::FunctionSignature {
    fn to_doc(&self) -> Document {
        let ast::FunctionSignature {
            type_parameters,
            parameters,
            return_type,
        } = self;
        // let type_parameters = TypeParameters(type_parameters.clone());
        let type_items = wrap_list("<", ">", ",", type_parameters.iter());
        let param_items = wrap_list(
            "(",
            ")",
            ",",
            parameters
                .iter()
                .map(|(n, t)| nil().append(n).append(": ").append(t)),
        );

        let ret = if let ast::Type_::Unit = &return_type.value {
            nil()
        } else {
            concats!(":", return_type)
        };
        group! {
            group!(type_items),
            param_items,
            ret
        }
    }
}

#[derive(Clone, Debug)]
struct TypeParameters(pub Vec<(Name, ast::Kind)>);

impl Documentable for TypeParameters {
    fn to_doc(&self) -> Document {
        if self.0.is_empty() {
            nil()
        } else {
            let parameters = concat(
                self.0
                    .iter()
                    .map(|p| p.to_doc())
                    .intersperse(pretty::delim(",")),
            );
            break_("", "").append(parameters)
            // wrap_args("<", ">", ",", self.0.iter().map(|p| p.to_doc()))
        }
    }
}

impl Documentable for (Name, ast::Kind) {
    fn to_doc(&self) -> Document {
        let (n, k) = self;
        use ast::Kind_ as K;
        let k = match k.value {
            K::Unknown => nil(),
            K::Resource => ": resource".to_doc(),
            K::Affine => ": copyable".to_doc(),
            K::Copyable => panic!("ICE 'copyable' kind constraint"),
        };
        concats!(n, k)
    }
}

impl Documentable for ast::ModuleIdent_ {
    fn to_doc(&self) -> Document {
        Document::Text(format!("{}::{}", &self.address, &self.name.0.value))
    }
}

impl Documentable for ast::ModuleAccess_ {
    fn to_doc(&self) -> Document {
        use ast::ModuleAccess_ as M;
        let s = match self {
            M::Name(n) => format!("{}", n),
            M::ModuleAccess(m, n) => format!("{}::{}", m, n),
            M::QualifiedModuleAccess(m, n) => format!("{}::{}", m, n),
        };
        s.to_doc()
    }
}

impl Documentable for Vec<ast::Use> {
    fn to_doc(&self) -> Document {
        concat(self.iter().map(|u| u.to_doc()).intersperse(line()))
    }
}

impl Documentable for ast::Use {
    fn to_doc(&self) -> Document {
        fn use_member_(member_name: &Name, alias: Option<&Name>) -> Document {
            let member = if let Some(n) = alias {
                format!("{} as {}", member_name, n)
            } else {
                format!("{}", member_name)
            };
            member.to_doc()
        }

        let doc = "use ".to_doc();
        match self {
            ast::Use::Module(module_ident, opt_name) => doc
                .append(&module_ident.0.value)
                .append(if let Some(name) = opt_name {
                    format!(" as {}", name).to_doc()
                } else {
                    nil()
                })
                .append(";"),
            ast::Use::Members(module_indent, members) => {
                let doc = concats!(doc, &module_indent.0.value, "::");
                let doc = if members.len() == 1 {
                    let (member_name, alias) = members.first().unwrap();
                    concats!(doc, use_member_(member_name, alias.as_ref()))
                } else if members.len() > 1 {
                    let members = wrap_args(
                        "{",
                        "}",
                        ",",
                        members
                            .into_iter()
                            .map(|(name, alias)| use_member_(name, alias.as_ref())),
                    );
                    concats!(doc, members)
                } else {
                    nil()
                };
                concats!(doc, ";")
            }
        }
    }
}

impl Documentable for ast::Constant {
    fn to_doc(&self) -> Document {
        let ast::Constant {
            signature,
            name,
            value,
            ..
        } = self;
        concats!("const ", &name.0, ": ", signature, " = ", value, ";")
    }
}

impl Documentable for Vec<ast::Type> {
    fn to_doc(&self) -> Document {
        let items = self.iter().map(|d| d.to_doc()).intersperse(delim(","));
        concat(items)
    }
}

impl Documentable for ast::Type_ {
    fn to_doc(&self) -> Document {
        match self {
            Type_::Unit => "()".to_doc(),
            Type_::Multiple(ss) => wrap_args("(", ")", ",", ss.iter().map(|d| d.to_doc())),
            Type_::Apply(module_access, ss) => {
                let tys = if ss.is_empty() {
                    nil()
                } else {
                    wrap_args("<", ">", ",", ss.iter().map(|t| t.to_doc()))
                };
                concats!(module_access.as_ref(), tys)
            }
            Type_::Ref(mut_, s) => {
                let prefix = if *mut_ { "&mut " } else { "&" };
                concats!(prefix, s.as_ref())
            }
            Type_::Fun(args, ret) => {
                wrap_args("(", "): ", ",", args.iter().map(|t| t.to_doc())).append(ret.as_ref())
            }
        }
    }
}

impl Documentable for ast::Exp_ {
    fn to_doc(&self) -> Document {
        use ast::Exp_ as E;
        match self {
            E::Unit => "()".to_doc(),
            E::Value(v) => v.to_doc(),
            E::InferredNum(u) => u.to_doc(),
            E::Move(v) => concats!("move ", v),
            E::Copy(v) => concats!("copy ", v),
            E::Name(ma, tys_opt) => {
                let tys = if let Some(ss) = tys_opt {
                    wrap_list("<", ">", ",", ss.iter())
                } else {
                    nil()
                };
                nil().append(ma).append(tys)
            }
            E::Call(ma, tys_opt, rhs) => {
                let tys = if let Some(ss) = tys_opt {
                    wrap_list("<", ">", ",", ss.iter()).group()
                } else {
                    nil()
                };
                nil()
                    .append(ma)
                    .append(tys)
                    .append(wrap_list("(", ")", ",", rhs.value.iter()))
                    .group()
            }
            E::Pack(ma, tys_opt, fields) => {
                let tys = if let Some(ss) = tys_opt {
                    wrap_list("<", ">", ",", ss.iter())
                } else {
                    nil()
                };
                // nil().append(ma).append(tys).append(
                //     nil().append("{").append(line())
                // )
                let fields = fields
                    .iter()
                    .map(|(f, e)| concats!(f, ": ", e))
                    .intersperse(line());
                let fields = concat(fields);
                let fields = group(concats!("{", indent!(2, fields), line(), "}"));
                concats!(ma, tys, fields)
            }
            E::IfElse(b, t, f_opt) => {
                let if_part = "if "
                    .to_doc()
                    .append(nest(2, concats!(break_("(", "("), b.as_ref())))
                    .append(break_(") ", ") "))
                    .append(t.as_ref().to_doc());
                let else_part = if let Some(f) = f_opt {
                    concats!(" else ", f.as_ref())
                } else {
                    nil()
                };
                if_part.append(else_part)
            }
            E::While(b, e) => "while "
                .to_doc()
                .append(nest(2, break_("(", "(").append(b.to_doc())))
                .append(break_(")", ")"))
                .append(e.to_doc()),
            E::Loop(e) => "loop ".to_doc().append(e.to_doc()),
            E::Block(seq) => seq.to_doc(),
            E::Lambda(bs, e) => {
                let bindlist = wrap_args("|", "|", ",", bs.value.iter().map(|b| b.to_doc()));
                concats!("fun ", bindlist, " ", e.as_ref())
            }
            E::ExpList(es) => wrap_args("(", ")", ",", es.iter().map(|e| e.to_doc())),
            E::Assign(lvalue, rhs) => concats!(lvalue.as_ref(), " = ", rhs.as_ref()),
            E::Return(e) => "return".to_doc().append(if let Some(v) = e {
                concats!(" ", v.as_ref())
            } else {
                nil()
            }),
            E::Abort(e) => concats!("abort ", e.as_ref()),
            E::Break => "break".to_doc(),
            E::Continue => "continue".to_doc(),
            E::Dereference(e) => concats!("*", e.as_ref()),
            E::UnaryExp(op, e) => concats!(op, e.as_ref()),
            E::BinopExp(l, op, r) => concats!(l.as_ref(), " ", op, " ", r.as_ref()),
            E::Borrow(mut_, e) => {
                let mut_sign = if *mut_ { "&mut " } else { "&" };
                concats!(mut_sign, e.as_ref())
            }
            E::Dot(e, n) => concats!(e.as_ref(), ".", n),
            E::Cast(e, ty) => concats!("(", e.as_ref(), " as ", ty, ")"),
            E::Index(e, i) => concats!(e.as_ref(), "[", i.as_ref(), "]"),
            E::Annotate(e, ty) => concats!("(", e.as_ref(), ": ", ty, ")"),
            E::Spec(_s) => todo!(),
            E::UnresolvedError => "_|_".to_doc(),
        }
    }
}

impl Documentable for ast::Sequence {
    fn to_doc(&self) -> Document {
        let (uses, items, _, exp) = self;
        let no_uses = uses.is_empty();
        let no_items = items.is_empty();
        let sequences = uses
            .into_iter()
            .map(|u| u.to_doc())
            .chain(items.iter().map(|i| i.to_doc()));

        let body = if let Some(e) = exp.as_ref() {
            concat(sequences.chain(vec![e.to_doc()]).intersperse(line()))
        } else {
            concat(sequences.intersperse(line()))
        };
        let line_or_break = if no_uses && no_items {
            break_("", "")
        } else {
            line()
        };

        group(
            "{".to_doc()
                .append(nest(2, line_or_break.clone().append(body)))
                .append(line_or_break)
                .append("}"),
        )
    }
}

impl Documentable for Vec<ast::SequenceItem> {
    fn to_doc(&self) -> Document {
        concat(self.iter().map(|s| s.to_doc()).intersperse(line()))
    }
}

impl Documentable for ast::SequenceItem_ {
    fn to_doc(&self) -> Document {
        use ast::SequenceItem_ as S;
        let doc = match self {
            S::Seq(e) => e.to_doc(),
            S::Bind(bs, ty_opt, e) => {
                let bs = if bs.value.len() == 1 {
                    bs.value.first().unwrap().to_doc()
                } else {
                    wrap_args("(", ")", ",", bs.value.iter().map(|b| b.to_doc()))
                };

                let ty_opt = if let Some(ty) = ty_opt {
                    ty.to_doc()
                } else {
                    nil()
                };
                concats!("let ", bs, ty_opt, " = ", e.as_ref())
            }
            S::Declare(bs, ty_opt) => {
                let bs = if bs.value.len() == 1 {
                    bs.value.first().unwrap().to_doc()
                } else {
                    wrap_args("(", ")", ",", bs.value.iter().map(|b| b.to_doc()))
                };

                let ty_opt = if let Some(ty) = ty_opt {
                    ty.to_doc()
                } else {
                    nil()
                };
                concats!("let ", bs, ty_opt)
            }
        };
        doc.append(";")
    }
}

impl Documentable for ast::Bind_ {
    fn to_doc(&self) -> Document {
        use ast::Bind_ as B;
        match self {
            B::Var(v) => v.to_doc(),
            B::Unpack(ma, tys_opt, fields) => {
                let tys_opt = if let Some(ss) = tys_opt {
                    wrap_args("<", ">", ",", ss.into_iter().map(|s| s.to_doc()))
                } else {
                    nil()
                };
                let fields = concat(fields.iter().map(|(f, b)| concats!(f, ": ", b, line())));
                let fields = nest(2, "{".to_doc().append(line()).append(fields));
                let fields = fields.append(line()).append("}");
                ma.to_doc().append(tys_opt).append(fields)
            }
        }
    }
}

impl Documentable for ast::UnaryOp_ {
    fn to_doc(&self) -> Document {
        use ast::UnaryOp_ as U;
        match self {
            U::Not => "!".to_doc(),
        }
    }
}
impl Documentable for ast::BinOp_ {
    fn to_doc(&self) -> Document {
        self.symbol().to_doc()
    }
}

macro_rules! document_name {
    ($n:ty) => {
        impl Documentable for $n {
            fn to_doc(&self) -> Document {
                self.0.to_doc()
            }
        }
    };
}

document_name!(ast::ModuleName);
document_name!(ast::Field);
document_name!(ast::StructName);
document_name!(ast::FunctionName);
document_name!(ast::ConstantName);
document_name!(ast::Var);

impl Documentable for ast::Value_ {
    fn to_doc(&self) -> Document {
        use ast::Value_ as V;
        let s = match self {
            V::Address(addr) => format!("{}", addr),
            V::U8(u) => format!("{}u8", u),
            V::U64(u) => format!("{}u64", u),
            V::U128(u) => format!("{}u128", u),
            V::Bool(b) => format!("{}", b),
            V::HexString(s) => format!("x\"{}\"", s),
            V::ByteString(s) => format!("b\"{}\"", s),
        };
        s.to_doc()
    }
}

impl<T> Documentable for move_ir_types::location::Spanned<T>
where
    T: Documentable,
{
    fn to_doc(&self) -> Document {
        self.value.to_doc()
    }
}

struct WrappedDocumentable<'f, 's, 'd, D: Documentable>
where
    's: 'f,
{
    formatter: &'f Formatter<'s>,
    doc: &'d Spanned<D>,
}

impl<'f, 's, 'd, D> WrappedDocumentable<'f, 's, 'd, D>
where
    D: Documentable,
    's: 'f,
{
    pub fn new(formatter: &'f Formatter<'s>, doc: &'d Spanned<D>) -> Self {
        Self { formatter, doc }
    }
}

impl<'f, 's, 'd, D> Documentable for WrappedDocumentable<'f, 's, 'd, D>
where
    D: Documentable,
    's: 'f,
{
    fn to_doc(&self) -> Document {
        let poped_comments = self
            .formatter
            .pop_doc_comments(self.doc.loc.span().start().to_usize());
        comments(poped_comments).append(&self.doc)
    }
}
