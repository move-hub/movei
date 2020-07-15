#[macro_use]
extern crate rpds;
#[macro_use]
extern crate log;

mod comments;
mod lang_items;
mod pretty;

pub use pretty::format;

use crate::{
    comments::{Comment, Comments},
    lang_items::LangItem,
    pretty::{
        break_, concat, delim, force_break, group, line, lines, nest, nil, space, Document,
        Documentable,
    },
};
use codespan::{ByteIndex, Span};
use itertools::Itertools;
use move_ir_types::location::Loc;
use move_lang::{
    parser::{
        ast,
        ast::{SpecBlockTarget_, Type_},
    },
    shared::{Address, Identifier, Name},
    FileCommentMap,
};
use std::{cell::RefCell, convert::TryFrom};

pub struct Formatter<'a> {
    inner: RefCell<Comments>,
    source: &'a str,
    indent: isize,
}

impl<'a> Formatter<'a> {
    pub fn new(source: &'a str, comment_map: FileCommentMap, indent: usize) -> Self {
        let comments = Comments::new(comment_map.keys().cloned().collect_vec());

        Self {
            inner: RefCell::new(comments),
            indent: indent as isize,
            source,
        }
    }

    // pop comments before the `span`
    fn pop_comments_before(&self, limit: usize) -> impl Iterator<Item = Comment> {
        self.pop_comments_between(0, limit)
    }

    // pop comments in span specified by `span_start` and `span_end`
    fn pop_comments_between(
        &self,
        span_start: usize,
        span_end: usize,
    ) -> impl Iterator<Item = Comment> {
        self.inner
            .borrow_mut()
            .pop_comments_between(Span::new(
                ByteIndex(span_start as u32),
                ByteIndex(span_end as u32),
            ))
            .map(move |s| Comment::try_from((s, self.source)).expect("Span should be comments"))
    }

    pub fn indent(&self) -> isize {
        self.indent
    }
}

macro_rules! cons {
    ($($doc:expr),*) => {{
        Document::Nil$(
            .append($doc)
        )*
    }};
}

impl<'a> Formatter<'a> {
    fn pretty_comments<'c>(&self, comments: impl Iterator<Item = Comment<'c>>) -> Option<Document> {
        let mut comments = comments.peekable();
        comments.peek()?;

        let mut doc = force_break();
        // keep the lines between two comments
        while let Some(c) = comments.next() {
            doc = doc.append(c.content);
            if let Some(next_comment) = comments.peek() {
                let distance = self.source
                    [c.span.end().to_usize()..next_comment.span.start().to_usize()]
                    .chars()
                    .filter(|c| c == &'\n')
                    .count();
                doc = doc.append(lines(distance));
            } else {
                // recheck the distance
                doc = doc.append(line());
            }
        }
        Some(doc)
    }

    pub fn definition(&self, expr: &ast::Definition) -> Document {
        let span = match expr {
            ast::Definition::Script(s) => s.loc.span(),
            ast::Definition::Module(m) => m.loc.span(),
            ast::Definition::Address(loc, _addr, _ms) => loc.span(),
        };

        let comments_before = self.pop_comments_before(span.start().to_usize());
        let comment_doc = self.pretty_comments(comments_before);

        let def = match expr {
            ast::Definition::Script(s) => self.script(s),
            ast::Definition::Module(m) => self.module(m),
            ast::Definition::Address(loc, addr, ms) => {
                // Tmp hack for address block Span.
                let span = Span::new(loc.span().start(), self.source.len() as u32);
                let loc = Loc::new(loc.file(), span);
                self.address_block(&loc, addr, ms)
            }
        };
        comment_doc.to_doc().append(def)
    }

    fn extract_container_comments(
        &self,
        container_span: Span,
        item_spans: impl Iterator<Item = Span>,
    ) -> Vec<Option<Document>> {
        let mut span_start = container_span.start().to_usize();
        let mut item_comments = Vec::new();

        for s in item_spans {
            let span_end = s.start().to_usize();
            let comments_before = self.pop_comments_between(span_start, span_end);
            let comments = self.pretty_comments(comments_before);
            item_comments.push(comments);
            span_start = s.end().to_usize();
        }

        let comments_after = self.pop_comments_between(span_start, container_span.end().to_usize());
        item_comments.push(self.pretty_comments(comments_after));
        item_comments
    }

    pub fn address_block(
        &self,
        loc: &Loc,
        addr: &Address,
        modules: &[ast::ModuleDefinition],
    ) -> Document {
        let header = "address ".to_doc().append(format!("{}", addr).to_doc());

        let mut item_comments =
            self.extract_container_comments(loc.span(), modules.iter().map(|m| m.loc.span()));
        let comments_after = item_comments.pop().unwrap();

        let module_items = item_comments.into_iter().zip(modules).map(|(comments, m)| {
            let module = self.module(m);
            comments.to_doc().append(module)
        });
        let modules = concat(module_items.intersperse(lines(2)));

        // no indent for address block
        let body = "{"
            .to_doc()
            .append(line())
            .append(modules)
            .append(line())
            .append(comments_after)
            .append("}");

        header.append(" ").append(body).group("address_block")
    }

    pub fn module(&self, module: &ast::ModuleDefinition) -> Document {
        use ast::ModuleMember as MM;
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

        let body = self.lang_items_(module.loc.span(), items.iter());
        let body = nest(self.indent, body.surround(line(), nil()));

        group(
            "module".to_string(),
            header.append(body).append(line()).append("}"),
        )
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

        let body = self.lang_items_(loc.span(), items.iter());
        nest(self.indent, line().append(body)).surround("script {", line().append("}"))
    }

    fn lang_items_<'b>(
        &self,
        container_span: Span,
        items: impl Iterator<Item = &'b LangItem<'b>>,
    ) -> Document {
        let mut peekable_items = items.peekable();

        let mut body = nil();
        let mut span_start = container_span.start().to_usize();
        while let Some(item) = peekable_items.next() {
            let loc = item.loc();
            let span_end = loc.span().start().to_usize();
            let comments_before = self.pop_comments_between(span_start, span_end);
            let comments_doc = self.pretty_comments(comments_before);

            let commented_item = comments_doc.to_doc().append(self.lang_item(item));
            body = body.append(commented_item);

            span_start = loc.span().end().to_usize();

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

        // maybe comments after last item
        let comments_after = self.pop_comments_between(span_start, container_span.end().to_usize());
        if let Some(last_comment) = self.pretty_comments(comments_after) {
            body.append(line()).append(last_comment)
        } else {
            body
        }
    }

    fn lang_item(&self, item: &LangItem) -> Document {
        match item {
            LangItem::Func(f) => self.fun_(f),
            LangItem::Constant(c) => self.constant_(c),
            LangItem::Use(u) => self.use_(u),
            LangItem::Struct(s) => self.struct_(s),
            LangItem::Spec(s) => self.spec_block_(s),
        }
    }

    //// Spec Block

    fn spec_block_(&self, s: &ast::SpecBlock) -> Document {
        use ast::SpecBlockTarget_ as T;
        let loc = s.loc;
        let ast::SpecBlock_ {
            target,
            uses,
            members,
        } = &s.value;

        let target = match &target.value {
            T::Code => nil(),

            SpecBlockTarget_::Module => "module".to_doc(),
            SpecBlockTarget_::Function(n) => "fun ".to_doc().append(n),
            SpecBlockTarget_::Structure(n) => "struct ".to_doc().append(n),
            SpecBlockTarget_::Schema(n, tys) => {
                let tys = if tys.is_empty() {
                    nil()
                } else {
                    wrap_list(
                        "<",
                        ">",
                        ",",
                        self.indent,
                        tys.iter().map(|p| self.type_parameter_(p)),
                    )
                    .group("schema_tys")
                };
                "schema ".to_doc().append(n).append(tys)
            }
        };
        let has_use = !uses.is_empty();

        // TODO: fix comments on uses.
        // in practice, it's rare to add comment on uses...
        let uses = uses.iter().map(|u| self.use_(u)).intersperse(line());

        let mut spec_member_comments =
            self.extract_container_comments(loc.span(), members.iter().map(|m| m.loc.span()));
        let comments_after = spec_member_comments.pop().unwrap();

        let spec_members = spec_member_comments
            .into_iter()
            .zip(members)
            .map(|(c, m)| c.to_doc().append(self.spec_member_(m)))
            .intersperse(line());

        let members = concat(spec_members);

        let sep_lines = if has_use { Some(lines(2)) } else { None };

        let items = concat(uses)
            .append(sep_lines)
            .append(members)
            .append(comments_after);

        let spec_body = line()
            .append(items)
            .nest(self.indent)
            .surround("{", line().append("}"));
        cons!("spec ", target, " ", spec_body)
    }

    fn spec_member_(&self, member: &ast::SpecBlockMember) -> Document {
        use ast::SpecBlockMember_ as M;
        match &member.value {
            M::Condition {
                kind,
                exp,
                properties,
            } => {
                let exp = self.exp_(exp);

                let properties = if properties.is_empty() {
                    None
                } else {
                    Some(
                        wrap_list(" [", "]", ",", self.indent, properties.iter())
                            .flex_break("properties"),
                    )
                };
                let breakable_exp = break_("", " ")
                    .append(exp.flex_break("exp"))
                    .nest(self.indent);
                cons!(kind, properties, breakable_exp, ";").group("spec_condition")
            }
            M::Function {
                uninterpreted,
                signature,
                name,
                body,
            } => {
                let modifier = if *uninterpreted {
                    "uninterpreted"
                } else if let ast::FunctionBody_::Native = &body.value {
                    "native"
                } else {
                    ""
                };
                let signature = self.fn_signature_(signature);
                let body = self.fn_body_(body);
                cons!(modifier, " ", "define", " ", name, signature, body).group("spec_function")
            }
            M::Variable {
                is_global,
                name,
                type_parameters,
                type_,
            } => {
                let modifier = if *is_global { "global" } else { "local" };

                let tys = self
                    .type_parameters_(type_parameters)
                    .unwrap_or_else(nil)
                    .group("tys");
                // group type_ to let tys break first.
                let type_ = self.type_(type_).group("type");
                cons!(modifier, " ", name, tys, ": ", type_, ";").group("spec_variable")
            }
            M::Include { exp } => {
                let exp = self.exp_(exp);
                cons!("include ", exp, ";").group("spec_include")
            }
            M::Apply {
                exp,
                patterns,
                exclusion_patterns,
            } => {
                let exp = self.exp_(exp);

                let patterns = if !patterns.is_empty() {
                    let patterns = concat(
                        patterns
                            .iter()
                            .map(|p| self.spec_apply_pattern_(p).group("pattern"))
                            .intersperse(delim(",")),
                    );
                    break_("", " ")
                        .append("to")
                        .append((break_("", " ")).append(patterns).nest(self.indent))
                        .flex_break("apply_patterns")
                } else {
                    nil()
                };

                let exclusion_patterns = if !exclusion_patterns.is_empty() {
                    let exclusion_patterns = concat(
                        exclusion_patterns
                            .iter()
                            .map(|p| self.spec_apply_pattern_(p).group("exclusion_pattern"))
                            .intersperse(delim(",")),
                    );
                    break_("", " ")
                        .append("except")
                        .append(
                            (break_("", " "))
                                .append(exclusion_patterns)
                                .nest(self.indent),
                        )
                        .flex_break("apply_exclusion_patterns")
                } else {
                    nil()
                };

                cons!(
                    "apply ",
                    exp.group("apply_exp"),
                    patterns,
                    exclusion_patterns,
                    ";"
                )
                .group("spec_apply")
            }
            M::Pragma { properties } => {
                let properties = concat(
                    properties
                        .iter()
                        .map(|p| p.to_doc())
                        .intersperse(delim(",")),
                );
                break_("pragma", "pragma ")
                    .append(properties)
                    .nest(self.indent)
                    .append(";")
                    .group("spec_pragma")
            }
        }
    }

    fn spec_apply_pattern_(&self, p: &ast::SpecApplyPattern) -> Document {
        let ast::SpecApplyPattern_ {
            visibility,
            name_pattern,

            type_parameters,
        } = &p.value;
        let visibility = match visibility {
            Some(ast::FunctionVisibility::Public(_)) => "public",
            Some(ast::FunctionVisibility::Internal) => "internal",
            None => "",
        };
        let name_pattern = concat(name_pattern.iter().map(|f| f.to_doc()));
        let tys = self.type_parameters_(type_parameters);
        cons!(visibility, name_pattern, tys)
    }

    fn struct_(&self, s: &ast::StructDefinition) -> Document {
        use ast::{StructDefinition as S, StructFields as SF};
        let S {
            resource_opt,
            name,
            type_parameters,
            fields,
            loc: struct_loc,
        } = s;

        let header = {
            let mut parts = vec![];
            if let SF::Native(_) = fields {
                parts.push("native".to_doc());
            }
            if resource_opt.is_some() {
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
                    " { }".to_doc()
                } else {
                    let mut comments = {
                        let fields_span = fs
                            .iter()
                            .map(|(f, t)| Span::new(f.0.loc.span().start(), t.loc.span().end()));
                        self.extract_container_comments(struct_loc.span(), fields_span)
                    };

                    let comment_after = comments.pop().unwrap();

                    let fs = {
                        let fs = comments
                            .into_iter()
                            .zip(fs)
                            .map(|(c, f)| c.to_doc().append(self.struct_field_(f)));
                        concat(fs.map(|f| f.append(",")).intersperse(line()))
                    };
                    // let fs = concat(fs.intersperse(",".to_doc().append(line())));
                    line()
                        .append(fs)
                        .append(comment_after)
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
                let doc = cons!(doc, &module_indent.0.value, "::");

                let doc = match members.len().cmp(&1) {
                    std::cmp::Ordering::Less => nil(),
                    std::cmp::Ordering::Equal => {
                        let (member_name, alias) = members.first().unwrap();
                        cons!(doc, use_member_(member_name, alias.as_ref()))
                    }
                    _ => {
                        let members = wrap_args(
                            "{",
                            "}",
                            ",",
                            self.indent,
                            members
                                .iter()
                                .map(|(name, alias)| use_member_(name, alias.as_ref())),
                        );
                        cons!(doc, members)
                    }
                };
                cons!(doc, ";")
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

    fn type_parameters_(&self, tys: &[(Name, ast::Kind)]) -> Option<Document> {
        if tys.is_empty() {
            None
        } else {
            let type_parameters = tys.iter().map(|t| self.type_parameter_(t));
            Some(wrap_list("<", ">", ",", self.indent, type_parameters))
        }
    }

    fn fn_signature_(&self, fn_signature: &ast::FunctionSignature) -> Document {
        let ast::FunctionSignature {
            type_parameters,
            parameters,
            return_type,
        } = fn_signature;
        let type_items = self.type_parameters_(type_parameters).unwrap_or_else(nil);

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
            cons!(": ", return_type)
        };
        cons! {
            type_items.flex_break("type_parameters".to_string()),
            param_items.flex_break("parameters".to_string()),
            ret.group("return_type")
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
        cons!(n, k)
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
            B::Defined(s) => " ".to_doc().append(self.sequence_(body.loc, s)),
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
        cons!("const ", &name.0, ": ", signature, " = ", value, ";")
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

                cons!(module_access.as_ref(), tys.flex_break("apply_type"))
            }
            Type_::Ref(mut_, s) => {
                let prefix = if *mut_ { "&mut " } else { "&" };
                let s = self.type_(s.as_ref());
                cons!(prefix, s)
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
        let loc = exp.loc;
        let exp_comments = self.pop_comments_before(loc.span().start().to_usize());
        let e = match &exp.value {
            E::Unit => "()".to_doc(),
            E::Value(v) => v.to_doc(),
            E::InferredNum(u) => u.to_doc(),
            E::Move(v) => cons!("move ", v),
            E::Copy(v) => cons!("copy ", v),
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
                            .iter()
                            .map(|f| {
                                let comments =
                                    self.pop_comments_before(f.0.loc().span().start().to_usize());
                                let comments = self.pretty_comments(comments);
                                let f = self.pack_field_(f);
                                cons!(comments, f)
                            })
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
                    cons!(" else ", f)
                } else {
                    nil()
                };
                cons!("if ", "(", b, ") ", t, else_part).flex_break("if-else")
            }
            E::While(b, e) => {
                let b = self.exp_(b.as_ref());
                let e = self.exp_(e.as_ref());
                cons!("while ", "(", b, ")", e)
                // "while "
                //     .to_doc()
                //     .append(nest(self.indent, break_("(", "(").append(b)))
                //     .append(break_(")", ")"))
                //     .append(e)
            }
            E::Loop(e) => {
                let e = self.exp_(e.as_ref());
                cons!("loop ", e)
            }
            E::Block(seq) => self.sequence_(exp.loc, seq),
            E::Lambda(bs, e) => {
                let bs = bs.value.iter().map(|b| self.bind_(b));
                let bindlist = wrap_list("|", "|", ",", self.indent, bs);
                let e = self.exp_(e.as_ref());
                cons!("fun ", bindlist, " ", e).flex_break("lambda")
            }
            E::ExpList(es) => {
                let es = es.iter().map(|e| self.exp_(e));
                wrap_list("(", ")", ",", self.indent, es).flex_break("tuple")
            }
            E::Assign(lvalue, rhs) => {
                let lvalue = self.exp_(lvalue.as_ref());
                let rhs = self.exp_(rhs.as_ref());
                cons!(lvalue, break_(" =", " = ").nest(self.indent), rhs).flex_break("assign")
            }
            E::Return(e) => {
                let e = e.as_ref().map(|e| self.exp_(e.as_ref()));
                if let Some(v) = e {
                    cons!("return ", v)
                } else {
                    "return".to_doc()
                }
            }
            E::Abort(e) => {
                let e = self.exp_(e.as_ref());
                cons!("abort ", e)
            }
            E::Break => "break".to_doc(),
            E::Continue => "continue".to_doc(),
            E::Dereference(e) => {
                let e = self.exp_(e.as_ref());
                cons!("*", e)
            }
            E::UnaryExp(op, e) => {
                let e = self.exp_(e.as_ref());
                cons!(op, e)
            }
            E::BinopExp(l, op, r) => {
                let l = self.exp_(l.as_ref());
                let r = self.exp_(r.as_ref());
                cons!(
                    l.flex_break("left_exp"),
                    " ",
                    op,
                    break_("", " ")
                        .append(r)
                        .nest(self.indent)
                        .flex_break("right_exp")
                )
            }
            E::Borrow(mut_, e) => {
                let mut_sign = if *mut_ { "&mut " } else { "&" };
                let e = self.exp_(e.as_ref());
                cons!(mut_sign, e)
            }
            E::Dot(e, n) => {
                let e = self.exp_(e.as_ref());
                cons!(e, ".", n)
            }
            E::Cast(e, ty) => {
                let e = self.exp_(e.as_ref());
                let ty = self.type_(ty);
                cons!("(", e, " as ", ty, ")")
            }
            E::Index(e, i) => {
                let e = self.exp_(e.as_ref());
                let i = self.exp_(i.as_ref());
                cons!(e, "[", i, "]")
            }
            E::Annotate(e, ty) => {
                let e = self.exp_(e.as_ref());
                let ty = self.type_(ty);
                cons!("(", e, ": ", ty, ")")
            }
            E::Spec(_s) => todo!(),
            E::UnresolvedError => "_|_".to_doc(),
        };
        commented(exp_comments, e)
    }

    fn pack_field_(&self, pack: &(ast::Field, ast::Exp)) -> Document {
        let (f, e) = pack;
        let f = f.to_doc();
        let exp = self.exp_(e);
        // short hand for struct pack
        if exp == f {
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
                    wrap_list("<", ">", ",", self.indent, ss.iter().map(|s| self.type_(s)))
                } else {
                    nil()
                };

                let fields = fields
                    .iter()
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

    fn sequence_(&self, loc: Loc, sequence: &ast::Sequence) -> Document {
        let (uses, items, _, exp) = sequence;

        let mut uses: Vec<_> = uses.iter().map(|u| self.use_(u)).collect();

        let (item_comments, exp_comment, comment_after) = {
            let mut spans: Vec<_> = items.iter().map(|i| i.loc.span()).collect();
            if let Some(e) = exp.as_ref() {
                spans.push(e.loc.span());
            }
            let mut comments = self.extract_container_comments(loc.span(), spans.into_iter());
            let comment_after = comments.pop().unwrap();
            let exp_comments = if exp.is_some() {
                comments.pop().unwrap()
            } else {
                None
            };
            (comments, exp_comments, comment_after)
        };

        let mut seq_items: Vec<_> = item_comments
            .into_iter()
            .zip(items)
            .map(|(c, i)| c.to_doc().append(self.sequence_item_(i)))
            .collect();

        uses.append(&mut seq_items);

        let mut sequences = uses;
        if let Some(e) = exp.as_ref() {
            let e = self.exp_(e).group("sequence_last_exp".to_string());
            sequences.push(exp_comment.to_doc().append(e));
        };
        if let Some(c) = comment_after {
            sequences.push(c);
        }

        if sequences.is_empty() {
            "{ }".to_doc()
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
                cons!("let ", bs, ty_opt, " = ", e)
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
                cons!("let ", bs, ty_opt)
            }
        };
        doc.append(";").group("sequence_item".to_string())
    }
}

fn commented<'a>(comments: impl Iterator<Item = Comment<'a>>, doc: Document) -> Document {
    match printed_comments(comments) {
        Some(comments) => comments.append(doc),
        _ => doc,
    }
}

fn printed_comments<'a>(items: impl Iterator<Item = Comment<'a>>) -> Option<Document> {
    let mut items = items.peekable();

    items.peek()?;
    Some(concat(items.map(|i| i.content.to_doc()).intersperse(line())).append(line()))
}

pub fn wrap_args<I>(open: &str, close: &str, delim: &str, indent: isize, args: I) -> Document
where
    I: Iterator<Item = Document>,
{
    wrap_list(open, close, delim, indent, args).group("wrap_args".to_string())
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
    if args.peek().is_none() {
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

impl Documentable for ast::SpecConditionKind {
    fn to_doc(&self) -> Document {
        use ast::SpecConditionKind::*;
        match self {
            Assert => "assert",
            Assume => "assume",
            Decreases => "decreases",
            AbortsIf => "aborts_if",
            SucceedsIf => "succeeds_if",
            Ensures => "ensures",
            Requires => "requires",
            RequiresModule => "requires module",
            Invariant => "invariant",
            InvariantUpdate => "invariant update",
            InvariantPack => "invariant pack",
            InvariantUnpack => "invariant unpack",
            InvariantModule => "invariant module",
        }
        .to_doc()
    }
}

impl Documentable for ast::SpecApplyFragment_ {
    fn to_doc(&self) -> Document {
        use ast::SpecApplyFragment_ as F;
        match self {
            F::Wildcard => "*".to_doc(),
            F::NamePart(n) => n.to_doc(),
        }
    }
}
impl Documentable for ast::PragmaProperty_ {
    fn to_doc(&self) -> Document {
        let value = self.value.as_ref().map(|v| " = ".to_doc().append(v));
        self.name.to_doc().append(value)
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
