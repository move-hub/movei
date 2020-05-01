use crate::resolver::StructFieldResolver;
use anyhow::{bail, Result};
use libra_types::account_address::AccountAddress;
use move_ir_types::location::Spanned;
use move_lang::{
    parser::ast as P,
    shared::{
        ast_debug::{AstDebug, AstWriter},
        Address, Name,
    },
};
use move_vm_types::{
    loaded_data::types::{FatStructType, FatType},
    values::{Struct, VMValueCast, Value},
};
use vm::errors::VMResult;

pub struct ResourceFormatter<'r> {
    resolver: &'r dyn StructFieldResolver,
}
impl<'r> ResourceFormatter<'r> {
    pub fn new(resolver: &'r dyn StructFieldResolver) -> Self {
        Self { resolver }
    }

    pub fn fmt(&self, t: &FatStructType, d: Struct) -> Result<String> {
        match vm_struct_to_parser_exp(self.resolver, t, d) {
            Ok(exp) => {
                let mut writer = AstWriter::normal();
                exp.ast_debug(&mut writer);
                Ok(format!("{}", writer))
            }
            Err(e) => bail!("fmt error: {:?}", e),
        }
    }
}

fn fat_type_to_parser_type(ty: FatType) -> P::Type {
    let t = match ty {
        FatType::Bool => {
            let module_access = P::ModuleAccess_::Name(Name::unsafe_no_loc("bool".to_string()));
            P::Type_::Apply(
                Box::new(P::ModuleAccess::unsafe_no_loc(module_access)),
                vec![],
            )
        }
        FatType::U8 => {
            let module_access = P::ModuleAccess_::Name(Name::unsafe_no_loc("u8".to_string()));
            P::Type_::Apply(
                Box::new(P::ModuleAccess::unsafe_no_loc(module_access)),
                vec![],
            )
        }
        FatType::U64 => {
            let module_access = P::ModuleAccess_::Name(Name::unsafe_no_loc("u64".to_string()));
            P::Type_::Apply(
                Box::new(P::ModuleAccess::unsafe_no_loc(module_access)),
                vec![],
            )
        }
        FatType::U128 => {
            let module_access = P::ModuleAccess_::Name(Name::unsafe_no_loc("u128".to_string()));
            P::Type_::Apply(
                Box::new(P::ModuleAccess::unsafe_no_loc(module_access)),
                vec![],
            )
        }
        FatType::Address => {
            let module_access = P::ModuleAccess_::Name(Name::unsafe_no_loc("address".to_string()));
            P::Type_::Apply(
                Box::new(P::ModuleAccess::unsafe_no_loc(module_access)),
                vec![],
            )
        }
        FatType::Vector(t) => {
            let module_access = P::ModuleAccess_::Name(Name::unsafe_no_loc("vector".to_string()));
            P::Type_::Apply(
                Box::new(P::ModuleAccess::unsafe_no_loc(module_access)),
                vec![fat_type_to_parser_type(*t)],
            )
        }
        FatType::Struct(s) => {
            let module_access = P::ModuleAccess_::QualifiedModuleAccess(
                P::ModuleIdent(Spanned::unsafe_no_loc(P::ModuleIdent_ {
                    address: Address::new(s.address.into()),
                    name: P::ModuleName(Name::unsafe_no_loc(s.module.to_string())),
                })),
                Name::unsafe_no_loc(s.name.to_string()),
            );
            let tys = s
                .ty_args
                .into_iter()
                .map(|t| fat_type_to_parser_type(t))
                .collect();
            P::Type_::Apply(Box::new(P::ModuleAccess::unsafe_no_loc(module_access)), tys)
        }
        _ => unreachable!(),
    };
    P::Type::unsafe_no_loc(t)
}

fn vm_struct_to_parser_exp(
    field_resolver: &dyn StructFieldResolver,
    t: &FatStructType,
    d: Struct,
) -> VMResult<P::Exp_> {
    let values = d.unpack()?;

    let mut fields = vec![];
    if !t.layout.is_empty() {
        for (i, (ty, v)) in t.layout.iter().zip(values).enumerate() {
            let field_name = field_resolver
                .resolve_field(&t.address, t.module.as_ref(), t.name.as_ref(), i)
                .expect("should resolve field");
            let f = P::Field(Name::unsafe_no_loc(field_name.as_str().to_string()));
            let exp = vm_value_to_parser_exp(field_resolver, ty, v)?;
            fields.push((f, P::Exp::unsafe_no_loc(exp)));
        }
    }
    let ty_args = t
        .ty_args
        .iter()
        .map(|t| fat_type_to_parser_type(t.clone()))
        .collect::<Vec<_>>();
    let ty_args = if ty_args.is_empty() {
        None
    } else {
        Some(ty_args)
    };
    let module_access = P::ModuleAccess::unsafe_no_loc(P::ModuleAccess_::QualifiedModuleAccess(
        P::ModuleIdent(Spanned::unsafe_no_loc(P::ModuleIdent_ {
            address: Address::new(t.address.into()),
            name: P::ModuleName(Name::unsafe_no_loc(t.module.to_string())),
        })),
        Name::unsafe_no_loc(t.name.to_string()),
    ));

    Ok(P::Exp_::Pack(module_access, ty_args, fields))
    // Ok(IR::Exp_::Pack(struct_name, ir_types, fields))
}

fn vm_value_to_parser_exp(
    field_resolver: &dyn StructFieldResolver,
    ty: &FatType,
    v: Value,
) -> VMResult<P::Exp_> {
    use P::{Exp, Exp_, Value_};
    let exp = match ty {
        FatType::Bool => {
            let v = v.cast()?;
            Exp_::Value(P::Value::unsafe_no_loc(Value_::Bool(v)))
        }
        FatType::U8 => {
            let v = v.cast()?;
            Exp_::Value(P::Value::unsafe_no_loc(Value_::U8(v)))
        }
        FatType::U64 => {
            let v = v.cast()?;
            Exp_::Value(P::Value::unsafe_no_loc(Value_::U64(v)))
        }
        FatType::U128 => {
            let v = v.cast()?;
            Exp_::Value(P::Value::unsafe_no_loc(Value_::U128(v)))
        }
        FatType::Address => {
            let v: AccountAddress = v.cast()?;
            Exp_::Value(P::Value::unsafe_no_loc(Value_::Address(Address::new(
                v.into(),
            ))))
        }
        FatType::Vector(t) => {
            match t.as_ref() {
                FatType::U8 => {
                    let v: Vec<u8> = v.cast()?;
                    Exp_::Value(P::Value::unsafe_no_loc(Value_::Bytearray(v)))
                }
                _ => {
                    let v: Vec<Value> = v.cast()?;
                    let mut exps = vec![];
                    for d in v {
                        let e = Exp::unsafe_no_loc(vm_value_to_parser_exp(
                            field_resolver,
                            t.as_ref(),
                            d,
                        )?);
                        exps.push(e);
                    }
                    // Use exp list to display vec
                    Exp_::ExpList(exps)
                }
            }
        }
        FatType::Struct(t) => {
            let v: Struct = v.cast()?;
            vm_struct_to_parser_exp(field_resolver, t.as_ref(), v)?
        }
        _ => unreachable!(),
    };
    Ok(exp)
}
