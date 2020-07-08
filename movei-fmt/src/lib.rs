#[macro_use]
extern crate im;
#[macro_use]
extern crate log;

mod lang_items;
mod pretty;

pub use pretty::format;

use crate::{
    lang_items::LangItem,
    pretty::{
        break_, concat, delim, force_break, group, line, lines, nest, nil, space, Document,
        Documentable,
    },
};
use codespan::Span;
use itertools::Itertools;
use move_ir_types::location::{Loc, Spanned};
use move_lang::{
    parser::{ast, ast::Type_},
    shared::{Address, Identifier, Name},
    FileCommentMap,
};
use std::cell::RefCell;

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
    #[allow(unused)]
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
    indent: isize,
}

impl<'a> Formatter<'a> {
    pub fn new(source: &'a str, comment_map: FileCommentMap, indent: usize) -> Self {
        let inner = Inner::new(source, comment_map);

        Self {
            inner: RefCell::new(inner),
            indent: indent as isize,
        }
    }

    fn pop_doc_comments(&self, limit: usize) -> impl Iterator<Item = Comment<'a>> {
        self.inner.borrow_mut().pop_doc_comments(limit)
    }

    pub fn indent(&self) -> isize {
        self.indent
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

macro_rules! concats {
    ($($doc:expr),*) => {{
        Document::Nil$(
            .append($doc)
        )*
    }};
}

impl<'a> Formatter<'a> {
    pub fn definition(&self, expr: &ast::Definition) -> Document {
        match expr {
            ast::Definition::Script(s) => self.script(s),
            ast::Definition::Module(m) => self.module(m),
            ast::Definition::Address(loc, addr, ms) => self.address_block(loc, addr, ms),
        }
    }

    pub fn address_block(
        &self,
        loc: &Loc,
        addr: &Address,
        modules: &[ast::ModuleDefinition],
    ) -> Document {
        let comments = comments(self.pop_doc_comments(loc.span().start().to_usize()));

        let header = "address ".to_doc().append(format!("{}", addr).to_doc());
        let modules = concat(modules.iter().map(|m| self.module(m)).intersperse(lines(2)));

        // no indent for address block
        let body = "{"
            .to_doc()
            .append(line())
            .append(modules)
            .append(line())
            .append("}");

        let address_block = group("address_block".to_string(), header.append(" ").append(body));
        comments.append(address_block)
    }

    pub fn module(&self, module: &ast::ModuleDefinition) -> Document {
        use ast::ModuleMember as MM;
        let module_comments = comments(self.pop_doc_comments(module.loc.span().start().to_usize()));

        let header = concat(
            vec!["module".to_doc(), module.name.to_doc(), "{".to_doc()]
                .into_iter()
                .intersperse(space()),
        );

        let items: Vec<_> = module
            .members
            .iter()
            .map(|member| match member {
                MM::Constant(c) => LangItem::Constant(c),
                MM::Use(u) => LangItem::Use(u),
                MM::Spec(s) => LangItem::Spec(s),
                MM::Struct(s) => LangItem::Struct(s),
                MM::Function(f) => LangItem::Func(f),
            })
            .collect();

        let body = self.lang_items_(items.iter());
        let body = nest(self.indent, body.surround(line(), nil()));

        let module = group(
            "module".to_string(),
            header.append(body).append(line()).append("}"),
        );
        module_comments.append(module)
    }

    pub fn script(&self, script: &ast::Script) -> Document {
        let ast::Script {
            loc,
            uses,
            constants,
            function,
            specs,
        } = script;
        let mut items = Vec::with_capacity(uses.len() + constants.len() + 1 + specs.len());
        items.extend(uses.iter().map(LangItem::Use));
        items.extend(constants.iter().map(LangItem::Constant));
        items.push(LangItem::Func(function));
        items.extend(specs.iter().map(LangItem::Spec));
        items.sort_by_key(|i| i.loc().span().start());

        let comments = comments(self.pop_doc_comments(loc.span().start().to_usize()));
        let body = self.lang_items_(items.iter());
        let body = nest(self.indent, line().append(body)).surround("script {", line().append("}"));

        comments.append(body)
    }

    fn lang_items_<'b>(&self, items: impl Iterator<Item = &'b LangItem<'b>>) -> Document {
        let mut peekable_items = items.peekable();
        let mut body = nil();
        while let Some(item) = peekable_items.next() {
            let item_comments =
                comments(self.pop_doc_comments(item.loc().span().start().to_usize()));
            body = body.append(item_comments).append(self.lang_item(item));
            if let Some(after) = peekable_items.peek() {
                match (item, after) {
                    (LangItem::Use(_), LangItem::Use(_)) => {
                        body = body.append(lines(1));
                    }
                    (LangItem::Use(_), _) => {
                        body = body.append(lines(2));
                    }
                    (LangItem::Constant(_), LangItem::Constant(_)) => {
                        body = body.append(lines(1));
                    }
                    (LangItem::Constant(_), _) => {
                        body = body.append(lines(2));
                    }
                    _ => {
                        body = body.append(lines(2));
                    }
                }
            }
        }
        body
    }

    fn lang_item(&self, item: &LangItem) -> Document {
        match item {
            LangItem::Func(f) => self.fun_(f),
            LangItem::Constant(c) => self.constant_(c),
            LangItem::Use(u) => self.use_(u),
            LangItem::Struct(s) => self.struct_(s),
            LangItem::Spec(_s) => nil(),
        }
    }

    fn struct_(&self, s: &ast::StructDefinition) -> Document {
        use ast::{StructDefinition as S, StructFields as SF};
        let S {
            resource_opt,
            name,
            type_parameters,
            fields,
            loc: _,
        } = s;

        let header = {
            let mut parts = vec![];
            if let SF::Native(_) = fields {
                parts.push("native".to_doc());
            }
            if let Some(_) = resource_opt {
                parts.push("resource".to_doc());
            }
            parts.push("struct".to_doc());
            parts.push(name.to_doc());

            concat(parts.into_iter().intersperse(space()))
        };
        let type_parameters = if type_parameters.is_empty() {
            nil()
        } else {
            wrap_list(
                "<",
                ">",
                ",",
                self.indent,
                type_parameters.iter().map(|p| self.type_parameter_(p)),
            )
        };

        let body = match fields {
            SF::Native(_) => ";".to_doc(),
            SF::Defined(fs) => {
                if fs.is_empty() {
                    " {}".to_doc()
                } else {
                    let fs = fs.iter().map(|f| {
                        let field_comments =
                            comments(self.pop_doc_comments(f.0.loc().span().start().to_usize()));
                        field_comments.append(self.struct_field_(f))
                    });
                    let fs = concat(fs.map(|f| f.append(",")).intersperse(line()));
                    // let fs = concat(fs.intersperse(",".to_doc().append(line())));
                    line()
                        .append(fs)
                        .nest(self.indent)
                        .surround(" {", line().append("}"))
                }
            }
        };

        group(
            "struct".to_string(),
            header.append(type_parameters).append(body),
        )
    }

    fn struct_field_(&self, f: &(ast::Field, ast::Type)) -> Document {
        let (n, t) = f;
        let t = self.type_(t);
        nil().append(n).append(": ").append(t)
    }

    fn use_(&self, use_stmt: &ast::Use) -> Document {
        fn use_member_(member_name: &Name, alias: Option<&Name>) -> Document {
            let member = if let Some(n) = alias {
                format!("{} as {}", member_name, n)
            } else {
                format!("{}", member_name)
            };
            member.to_doc()
        }

        let doc = "use ".to_doc();
        match use_stmt {
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
                        self.indent,
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

    fn fun_(&self, function: &ast::Function) -> Document {
        let ast::Function {
            visibility,
            signature,
            acquires,
            name,
            body,
            ..
        } = function;
        let visibility = {
            use ast::FunctionVisibility as F;
            match visibility {
                F::Internal => nil(),
                F::Public(_) => "public ".to_doc(),
            }
        };
        let native_opt = if let ast::FunctionBody_::Native = &body.value {
            "native ".to_doc()
        } else {
            nil()
        };

        let func = visibility
            .append(native_opt)
            .append("fun ")
            .append(name)
            .append(self.fn_signature_(signature))
            .append(self.fn_acquires_(acquires))
            .append(self.fn_body_(body));

        func.group("func".to_string())
    }

    fn fn_signature_(&self, fn_signature: &ast::FunctionSignature) -> Document {
        let ast::FunctionSignature {
            type_parameters,
            parameters,
            return_type,
        } = fn_signature;
        let type_items = if type_parameters.is_empty() {
            nil()
        } else {
            let type_parameters = type_parameters.iter().map(|t| self.type_parameter_(t));
            wrap_list("<", ">", ",", self.indent, type_parameters)
        };

        let param_items = wrap_list(
            "(",
            ")",
            ",",
            self.indent,
            parameters.iter().map(|p| self.fn_parameter_(p)),
        );

        let ret = if let ast::Type_::Unit = &return_type.value {
            nil()
        } else {
            let return_type = self.type_(return_type);
            concats!(": ", return_type)
        };
        concats! {
            type_items.flex_break("type_parameters".to_string()),
            param_items.flex_break("parameters".to_string()),
            ret
        }
    }
    fn type_parameter_(&self, type_parameter: &(Name, ast::Kind)) -> Document {
        let (n, k) = type_parameter;
        use ast::Kind_ as K;
        let k = match k.value {
            K::Unknown => nil(),
            K::Resource => ": resource".to_doc(),
            K::Affine => ": copyable".to_doc(),
            K::Copyable => panic!("ICE 'copyable' kind constraint"),
        };
        concats!(n, k)
    }
    fn fn_parameter_(&self, parameter: &(ast::Var, ast::Type)) -> Document {
        let (n, t) = parameter;
        let t = self.type_(t);
        nil().append(n).append(": ").append(t)
    }

    fn fn_acquires_(&self, acquires: &[ast::ModuleAccess]) -> Document {
        if acquires.is_empty() {
            return nil();
        };
        let acquires = wrap_list(
            "",
            "",
            ",",
            self.indent,
            acquires.iter().map(|ma| ma.to_doc()),
        );
        break_("", " ")
            .append("acquires ")
            .append(acquires.flex_break("acquires"))
            .flex_break("fn_acquires")
    }

    fn fn_body_(&self, body: &ast::FunctionBody) -> Document {
        use ast::FunctionBody_ as B;
        match &body.value {
            B::Native => ";".to_doc(),
            B::Defined(s) => " ".to_doc().append(self.sequence_(s)),
        }
    }

    fn constant_(&self, constant: &ast::Constant) -> Document {
        let ast::Constant {
            signature,
            name,
            value,
            ..
        } = constant;
        let signature = self.type_(signature);
        let value = self.exp_(value);
        concats!("const ", &name.0, ": ", signature, " = ", value, ";")
    }

    fn type_(&self, ty: &ast::Type) -> Document {
        match &ty.value {
            Type_::Unit => "()".to_doc(),
            Type_::Multiple(ss) => {
                let ss = ss.iter().map(|d| self.type_(d));
                wrap_list("(", ")", ",", self.indent, ss).flex_break("tuple_type".to_string())
            }
            Type_::Apply(module_access, ss) => {
                let tys = if ss.is_empty() {
                    nil()
                } else {
                    let ss = ss.iter().map(|d| self.type_(d));
                    wrap_list("<", ">", ",", self.indent, ss)
                };

                concats!(module_access.as_ref(), tys.flex_break("apply_type"))
            }
            Type_::Ref(mut_, s) => {
                let prefix = if *mut_ { "&mut " } else { "&" };
                let s = self.type_(s.as_ref());
                concats!(prefix, s)
            }
            Type_::Fun(args, ret) => {
                let args = args.iter().map(|t| self.type_(t));
                let args = wrap_list("|", "|", ",", self.indent, args);
                let ret = self.type_(ret.as_ref());
                args.append(": ").append(ret)
            }
        }
    }

    fn exp_(&self, exp: &ast::Exp) -> Document {
        use ast::Exp_ as E;
        match &exp.value {
            E::Unit => "()".to_doc(),
            E::Value(v) => v.to_doc(),
            E::InferredNum(u) => u.to_doc(),
            E::Move(v) => concats!("move ", v),
            E::Copy(v) => concats!("copy ", v),
            E::Name(ma, tys_opt) => {
                let tys = if let Some(ss) = tys_opt {
                    wrap_list("<", ">", ",", self.indent, ss.iter().map(|s| self.type_(s)))
                        .flex_break("type_parameters")
                } else {
                    nil()
                };
                nil().append(ma).append(tys)
            }
            E::Call(ma, tys_opt, rhs) => {
                let tys = if let Some(ss) = tys_opt {
                    wrap_list("<", ">", ",", self.indent, ss.iter().map(|s| self.type_(s)))
                        .flex_break("type_arguments".to_string())
                } else {
                    nil()
                };
                let rhs = wrap_list(
                    "(",
                    ")",
                    ",",
                    self.indent,
                    rhs.value.iter().map(|e| self.exp_(e)),
                )
                .flex_break("call_args");

                nil().append(ma).append(tys).append(rhs)
            }
            E::Pack(ma, tys_opt, fields) => {
                let tys = if let Some(ss) = tys_opt {
                    wrap_list("<", ">", ",", self.indent, ss.iter().map(|s| self.type_(s)))
                } else {
                    nil()
                };
                let fields = if fields.is_empty() {
                    "{}".to_doc()
                } else {
                    let fields = concat(
                        fields
                            .into_iter()
                            .map(|f| self.pack_field_(f))
                            .intersperse(break_(",", ", ")),
                    );
                    break_("{", "{ ")
                        .append(fields)
                        .nest(self.indent)
                        .append(break_(",", " "))
                        .append("}")
                };
                ma.to_doc()
                    .append(tys.flex_break("types"))
                    .append(" ")
                    .append(fields.flex_break("fields"))
            }
            E::IfElse(b, t, f_opt) => {
                let b = self.exp_(b.as_ref());
                let t = self.exp_(t.as_ref());
                let f_opt = f_opt.as_ref().map(|f| self.exp_(f.as_ref()));

                // let if_part = "if "
                //     .to_doc()
                //     .append(nest(self.indent, concats!(break_("(", "("), b)))
                //     .append(break_(") ", ") "))
                //     .append(t);
                //
                let else_part = if let Some(f) = f_opt {
                    concats!(" else ", f)
                } else {
                    nil()
                };
                concats!("if ", "(", b, ") ", t, else_part).flex_break("if-else")
            }
            E::While(b, e) => {
                let b = self.exp_(b.as_ref());
                let e = self.exp_(e.as_ref());
                concats!("while ", "(", b, ")", e)
                // "while "
                //     .to_doc()
                //     .append(nest(self.indent, break_("(", "(").append(b)))
                //     .append(break_(")", ")"))
                //     .append(e)
            }
            E::Loop(e) => {
                let e = self.exp_(e.as_ref());
                concats!("loop ", e)
            }
            E::Block(seq) => self.sequence_(seq),
            E::Lambda(bs, e) => {
                let bs = bs.value.iter().map(|b| self.bind_(b));
                let bindlist = wrap_list("|", "|", ",", self.indent, bs);
                let e = self.exp_(e.as_ref());
                concats!("fun ", bindlist, " ", e).flex_break("lambda")
            }
            E::ExpList(es) => {
                let es = es.iter().map(|e| self.exp_(e));
                wrap_list("(", ")", ",", self.indent, es).flex_break("tuple")
            }
            E::Assign(lvalue, rhs) => {
                let lvalue = self.exp_(lvalue.as_ref());
                let rhs = self.exp_(rhs.as_ref());
                concats!(lvalue, break_(" =", " = ").nest(self.indent), rhs).flex_break("assign")
            }
            E::Return(e) => {
                let e = e.as_ref().map(|e| self.exp_(e.as_ref()));
                if let Some(v) = e {
                    concats!("return ", v)
                } else {
                    "return".to_doc()
                }
            }
            E::Abort(e) => {
                let e = self.exp_(e.as_ref());
                concats!("abort ", e)
            }
            E::Break => "break".to_doc(),
            E::Continue => "continue".to_doc(),
            E::Dereference(e) => {
                let e = self.exp_(e.as_ref());
                concats!("*", e)
            }
            E::UnaryExp(op, e) => {
                let e = self.exp_(e.as_ref());
                concats!(op, e)
            }
            E::BinopExp(l, op, r) => {
                let l = self.exp_(l.as_ref());
                let r = self.exp_(r.as_ref());
                concats!(
                    l.flex_break("left_exp"),
                    " ",
                    op,
                    break_("", " ").nest(self.indent),
                    r.flex_break("right_exp")
                )
            }
            E::Borrow(mut_, e) => {
                let mut_sign = if *mut_ { "&mut " } else { "&" };
                let e = self.exp_(e.as_ref());
                concats!(mut_sign, e)
            }
            E::Dot(e, n) => {
                let e = self.exp_(e.as_ref());
                concats!(e, ".", n)
            }
            E::Cast(e, ty) => {
                let e = self.exp_(e.as_ref());
                let ty = self.type_(ty);
                concats!("(", e, " as ", ty, ")")
            }
            E::Index(e, i) => {
                let e = self.exp_(e.as_ref());
                let i = self.exp_(i.as_ref());
                concats!(e, "[", i, "]")
            }
            E::Annotate(e, ty) => {
                let e = self.exp_(e.as_ref());
                let ty = self.type_(ty);
                concats!("(", e, ": ", ty, ")")
            }
            E::Spec(_s) => todo!(),
            E::UnresolvedError => "_|_".to_doc(),
        }
    }

    fn pack_field_(&self, pack: &(ast::Field, ast::Exp)) -> Document {
        let (f, e) = pack;
        let f = f.to_doc();
        let exp = self.exp_(e);
        // short hand for struct pack
        if &exp == &f {
            f
        } else {
            f.append(": ").append(exp)
        }
    }

    fn bind_(&self, bind: &ast::Bind) -> Document {
        use ast::Bind_ as B;
        match &bind.value {
            B::Var(v) => v.to_doc(),
            B::Unpack(ma, tys_opt, fields) => {
                let tys_opt = if let Some(ss) = tys_opt {
                    wrap_list(
                        "<",
                        ">",
                        ",",
                        self.indent,
                        ss.into_iter().map(|s| self.type_(s)),
                    )
                } else {
                    nil()
                };

                let fields = fields
                    .into_iter()
                    .map(|(f, b)| f.to_doc().append(": ").append(self.bind_(b)))
                    .intersperse(break_(",", ", "));
                let fields = break_("{", "{ ")
                    .append(concat(fields))
                    .nest(self.indent)
                    .append(break_(",", " "))
                    .append("}");

                ma.to_doc()
                    .append(tys_opt.flex_break("type_arguments"))
                    .append(fields.flex_break("bind_fields"))
            }
        }
    }

    fn sequence_(&self, sequence: &ast::Sequence) -> Document {
        let (uses, items, _, exp) = sequence;

        let mut sequences: Vec<_> = uses
            .into_iter()
            .map(|u| self.use_(u))
            .chain(items.iter().map(|i| {
                // TODO: fix regular comments
                let _ = self.pop_doc_comments(i.loc.span().start().to_usize());
                self.sequence_item_(i)
            }))
            .collect();

        if let Some(e) = exp.as_ref() {
            sequences.push(self.exp_(e).group("sequence_last_exp".to_string()));
        };
        if sequences.is_empty() {
            "{ }".to_doc()
        } else if sequences.len() == 1 {
            let body = concat(sequences.into_iter().intersperse(line()));
            break_("{", "{ ")
                .append(body)
                .nest(self.indent)
                .append(break_("", " "))
                .append("}")
                .flex_break("sequence".to_string())
        } else {
            let body = concat(sequences.into_iter().intersperse(line()));
            line()
                .append(body)
                .nest(self.indent)
                .surround("{", line().append("}"))
                .group("sequence".to_string())
        }
    }

    fn sequence_item_(&self, item: &ast::SequenceItem) -> Document {
        use ast::SequenceItem_ as S;
        let doc = match &item.value {
            S::Seq(e) => self.exp_(e.as_ref()),
            S::Bind(bs, ty_opt, e) => {
                let bs = if bs.value.len() == 1 {
                    self.bind_(bs.value.first().unwrap())
                } else {
                    wrap_args(
                        "(",
                        ")",
                        ",",
                        self.indent,
                        bs.value.iter().map(|b| self.bind_(b)),
                    )
                };

                let ty_opt = if let Some(ty) = ty_opt {
                    self.type_(ty)
                } else {
                    nil()
                };
                let e = self.exp_(e.as_ref());
                concats!("let ", bs, ty_opt, " = ", e)
            }
            S::Declare(bs, ty_opt) => {
                let bs = if bs.value.len() == 1 {
                    self.bind_(bs.value.first().unwrap())
                } else {
                    wrap_args(
                        "(",
                        ")",
                        ",",
                        self.indent,
                        bs.value.iter().map(|b| self.bind_(b)),
                    )
                };

                let ty_opt = if let Some(ty) = ty_opt {
                    self.type_(ty)
                } else {
                    nil()
                };
                concats!("let ", bs, ty_opt)
            }
        };
        doc.append(";").group("sequence_item".to_string())
    }
}

fn comments<'a>(items: impl Iterator<Item = Comment<'a>>) -> Document {
    let mut items = items.peekable();

    if items.peek().is_none() {
        return nil();
    }
    concat(items.map(|i| i.content.to_doc()).intersperse(line())).append(line())
}

pub fn wrap_args<I>(open: &str, close: &str, delim: &str, indent: isize, args: I) -> Document
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
        .nest(indent)
        .append(break_(delim, ""))
        .append(close)
        .group("wrap_args".to_string())
}

pub fn wrap_list<I, D: Documentable>(
    open: &str,
    close: &str,
    delim: &str,
    indent: isize,
    args: I,
) -> Document
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
        .nest(indent)
        .append(break_(delim, ""))
        .append(close)
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
