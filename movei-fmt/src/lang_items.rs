use move_ir_types::location::Loc;
use move_lang::parser::ast::{Constant, Function, SpecBlock, StructDefinition, Use};

#[derive(Debug, PartialEq)]
pub enum LangItem<'a> {
    Use(&'a Use),
    Constant(&'a Constant),
    Func(&'a Function),
    Spec(&'a SpecBlock),
    Struct(&'a StructDefinition),
}

impl<'a> LangItem<'a> {
    pub fn loc(&self) -> Loc {
        match *self {
            // FIXME: make Use has correct loc.
            LangItem::Use(u) => match u {
                Use::Module(m, _) => m.loc(),
                Use::Members(m, _) => m.loc(),
            },
            LangItem::Constant(c) => c.loc,
            LangItem::Func(f) => f.loc,
            LangItem::Spec(s) => s.loc,
            LangItem::Struct(s) => s.loc,
        }
    }
}
