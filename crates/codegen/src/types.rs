use core::fmt;
use std::{slice, str};

use inkwell::{
    context::Context,
    types::{BasicMetadataTypeEnum, BasicTypeEnum, FunctionType, StructType},
    AddressSpace,
};
use parser::{AsTypeId, TypeId};
use typeck::Type;

use crate::{context, ModuleGen};

//

#[repr(C)]
pub struct Str {
    len: usize,
    ptr: *const u8,
}

impl Str {
    pub fn as_str(&self) -> &str {
        let str_slice = unsafe { slice::from_raw_parts(self.ptr, self.len) };
        str::from_utf8(str_slice).unwrap()
    }
}

impl fmt::Display for Str {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl AsTypeId for Str {
    const TYPE_ID: TypeId = TypeId::Str;
}

//

pub trait FnAsLlvm {
    fn return_type(&self) -> TypeId;

    fn args(&self) -> &[TypeId];

    fn as_extern_c_fn_ptr(&self) -> usize;
}

impl<R: AsTypeId> FnAsLlvm for extern "C" fn() -> R {
    fn return_type(&self) -> TypeId {
        R::TYPE_ID
    }

    fn args(&self) -> &[TypeId] {
        &[]
    }

    fn as_extern_c_fn_ptr(&self) -> usize {
        *self as usize
    }
}

impl<A: AsTypeId, R: AsTypeId> FnAsLlvm for extern "C" fn(A) -> R {
    fn return_type(&self) -> TypeId {
        R::TYPE_ID
    }

    fn args(&self) -> &[TypeId] {
        &[A::TYPE_ID]
    }

    fn as_extern_c_fn_ptr(&self) -> usize {
        *self as usize
    }
}

impl<A: AsTypeId, B: AsTypeId, R: AsTypeId> FnAsLlvm for extern "C" fn(A, B) -> R {
    fn return_type(&self) -> TypeId {
        R::TYPE_ID
    }

    fn args(&self) -> &[TypeId] {
        &[A::TYPE_ID, B::TYPE_ID]
    }

    fn as_extern_c_fn_ptr(&self) -> usize {
        *self as usize
    }
}

//

pub trait AsLlvm {
    // fn as_llvm(&self, types: &typeck::Context) -> BasicTypeEnum<'static>;

    fn as_llvm_fn(
        &self,
        gen: &ModuleGen,
        param_types: &[BasicMetadataTypeEnum<'static>],
        is_var_args: bool,
    ) -> FunctionType<'static>;

    fn as_llvm(&self, gen: &ModuleGen) -> Option<BasicTypeEnum<'static>>;

    fn as_llvm_meta(&self, gen: &ModuleGen) -> Option<BasicMetadataTypeEnum<'static>>;
}

impl AsLlvm for TypeId {
    fn as_llvm_fn(
        &self,
        gen: &ModuleGen,
        param_types: &[BasicMetadataTypeEnum<'static>],
        is_var_args: bool,
    ) -> FunctionType<'static> {
        let ctx = context();
        match gen.types.get_type(*self) {
            Type::I32 => ctx.i32_type().fn_type(param_types, is_var_args),
            Type::Str => todo!(),
            Type::Void => ctx.void_type().fn_type(param_types, is_var_args),
            Type::Func(_f) => ctx.void_type().fn_type(param_types, is_var_args),
        }
    }

    fn as_llvm(&self, gen: &ModuleGen) -> Option<BasicTypeEnum<'static>> {
        let ctx = context();
        match gen.types.get_type(*self) {
            Type::I32 => Some(ctx.i32_type().into()),
            Type::Str => Some(
                get_or_init_struct(ctx, "str", |s| {
                    s.set_body(
                        &[
                            ctx.ptr_sized_int_type(gen.engine.get_target_data(), None)
                                .into(),
                            ctx.i8_type().ptr_type(AddressSpace::default()).into(),
                        ],
                        false,
                    );
                })
                .into(),
            ),
            Type::Void => None,
            Type::Func(_func_id) => None, // Some(get_or_init_struct(ctx, &format!("[anon_func_{}]", func_id.0)).into()),
        }
    }

    fn as_llvm_meta(&self, gen: &ModuleGen) -> Option<BasicMetadataTypeEnum<'static>> {
        let ctx = context();
        match gen.types.get_type(*self) {
            Type::I32 => Some(ctx.i32_type().into()),
            Type::Str => Some(
                get_or_init_struct(ctx, "str", |s| {
                    s.set_body(
                        &[
                            ctx.ptr_sized_int_type(gen.engine.get_target_data(), None)
                                .into(),
                            ctx.i8_type().ptr_type(AddressSpace::default()).into(),
                        ],
                        false,
                    );
                })
                .into(),
            ),
            Type::Void => None,
            Type::Func(_func_id) => None, // Some(get_or_init_struct(ctx, &format!("[anon_func_{}]", func_id.0)).into()),
        }
    }
}

pub fn get_or_init_struct<'a>(
    ctx: &'a Context,
    name: &str,
    setup_body: impl FnOnce(StructType<'a>),
) -> StructType<'a> {
    if let Some(s) = ctx.get_struct_type(name) {
        s
    } else {
        let s = ctx.opaque_struct_type(name);
        setup_body(s);
        s
    }
}

//

// use std::{
//     collections::{hash_map::Entry, BTreeSet, BinaryHeap, HashMap, HashSet},
//     rc::Rc,
// };

// use inkwell::{
//     types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, IntType, VoidType},
//     values::{BasicMetadataValueEnum, BasicValueEnum, IntValue, StructValue},
// };

// use crate::context;

// //

// pub struct Types {}

// impl Types {
//     pub const fn new() -> Self {
//         Self {}
//     }

//     pub fn new_struct(&mut self, name: &str) -> StructBuilder {
//         StructBuilder {
//             fields: <_>::default(),
//             field_names: <_>::default(),
//         }
//     }
// }

// //

// pub struct StructBuilder {
//     fields: Vec<BasicTypeEnum<'static>>,
//     field_names: HashMap<Rc<str>, Type>,
// }

// impl StructBuilder {
//     // pub fn align(self, _alignment: usize) -> Self {
//     //     self
//     // }

//     pub fn field(mut self, name: &str, ty: impl Into<Type>) -> Self {
//         let name: Rc<str> = Rc::from(name);
//         let ty: Type = ty.into();
//         let ll_ty = ty.as_basic_type_enum();

//         match self.field_names.entry(name.clone()) {
//             Entry::Occupied(entry) => panic!("field name collision"),
//             Entry::Vacant(entry) => entry.insert(ty),
//         };

//         if let Some(ty) = ll_ty {
//             self.fields.push(ty);
//         }

//         self
//     }

//     pub fn build(self) -> Struct {
//         let ll_ty = context().struct_type(&self.fields[..], false);
//         Struct {
//             fields: self.field_names.into(),
//             ll_ty,
//         }
//     }
// }

// //

// #[derive(Debug, Clone)]
// pub struct Struct {
//     fields: Rc<HashMap<Rc<str>, Type>>,
//     ll_ty: inkwell::types::StructType<'static>,
// }

// impl Struct {
//     pub fn size(&self) -> usize {
//         // self.ll_ty.size_of()
//         // todo!()
//         0
//     }

//     pub fn instance(&self) -> InstanceBuilder {
//         InstanceBuilder {
//             base: self.clone(),
//             fields: <_>::default(),
//             field_names: <_>::default(),
//         }
//     }

//     pub const fn as_basic_metadata_type_enum(&self) -> Option<BasicMetadataTypeEnum<'static>> {
//         Some(BasicMetadataTypeEnum::StructType(self.ll_ty))
//     }

//     pub const fn as_basic_type_enum(&self) -> Option<BasicTypeEnum<'static>> {
//         Some(BasicTypeEnum::StructType(self.ll_ty))
//     }
// }

// impl From<Struct> for Type {
//     fn from(value: Struct) -> Self {
//         Type::Struct(value)
//     }
// }

// //

// pub struct InstanceBuilder {
//     base: Struct,
//     fields: Vec<BasicValueEnum<'static>>,
//     field_names: HashSet<Rc<str>>,
// }

// impl InstanceBuilder {
//     pub fn field(mut self, name: &str, val: impl Into<Value>) -> Self {
//         let Some(field_ty) = self.base.fields.get(name) else {
//             panic!("unknown field");
//         };

//         let name: Rc<str> = Rc::from(name);
//         let val: Value = val.into();
//         let ll_val = val.as_basic_value_enum();

//         if field_ty != &val.ty() {
//             panic!("field type mismatch")
//         }

//         if !self.field_names.insert(name) {
//             panic!("field name collision")
//         }

//         if let Some(val) = ll_val {
//             self.fields.push(val);
//         }

//         self
//     }

//     pub fn build(self) -> Instance {
//         for field in self.base.fields.keys() {
//             if !self.field_names.contains(field) {
//                 panic!("missing field {field}");
//             }
//         }

//         let ll_val = self.base.ll_ty.const_named_struct(&self.fields[..]);
//         Instance {
//             ty: self.base,
//             ll_val,
//         }
//     }
// }

// //

// #[derive(Debug, Clone)]
// pub struct Instance {
//     ty: Struct,
//     ll_val: StructValue<'static>,
// }

// impl Instance {
//     pub const fn as_basic_metadata_value_enum(&self) -> Option<BasicMetadataValueEnum<'static>> {
//         Some(BasicMetadataValueEnum::StructValue(self.ll_val))
//     }

//     pub const fn as_basic_value_enum(&self) -> Option<BasicValueEnum<'static>> {
//         Some(BasicValueEnum::StructValue(self.ll_val))
//     }
// }

// impl From<Instance> for Value {
//     fn from(value: Instance) -> Self {
//         Value::Struct(value)
//     }
// }

// //

// // pub struct Field {
// //     name:
// // }

// //

// #[derive(Debug, Clone)]
// pub enum Value {
//     I32(I32Value),
//     Void(VoidValue),
//     Struct(Instance),
// }

// impl Value {
//     pub fn ty(&self) -> Type {
//         match self {
//             Value::I32(i) => Type::I32(i.0),
//             Value::Void(v) => Type::Void(v.0),
//             Value::Struct(s) => Type::Struct(s.ty.clone()),
//         }
//     }

//     pub fn as_basic_value_enum(&self) -> Option<BasicValueEnum<'static>> {
//         match self {
//             Value::I32(i) => Some(BasicValueEnum::IntValue(i.1)),
//             Value::Void(_) => None,
//             Value::Struct(s) => Some(BasicValueEnum::StructValue(s.ll_val)),
//         }
//     }
// }

// //

// #[derive(Debug, Clone)]
// pub enum Type {
//     // Primitive(Primitive),
//     I32(I32),
//     Void(Void),
//     Struct(Struct),
// }

// impl Eq for Type {}

// impl PartialEq for Type {
//     fn eq(&self, other: &Self) -> bool {
//         match (self, other) {
//             (Type::I32(_), Type::I32(_)) => true,
//             (Type::Void(_), Type::Void(_)) => true,
//             (Type::Struct(lhs), Type::Struct(rhs)) => Rc::ptr_eq(&lhs.fields, &rhs.fields),
//             _ => false,
//         }
//     }
// }

// // impl From<Primitive> for Type {
// //     fn from(value: Primitive) -> Self {
// //         Type::Primitive(value)
// //     }
// // }

// impl Type {
//     // pub fn i32() -> Self {
//     //     Self::Primitive(Primitive::i32())
//     // }

//     // pub fn none() -> Self {
//     //     Self::Primitive(Primitive::none())
//     // }

//     pub fn as_basic_metadata_type_enum(&self) -> Option<BasicMetadataTypeEnum<'static>> {
//         match self {
//             Type::I32(i) => Some(BasicMetadataTypeEnum::IntType(i.0)),
//             Type::Void(_) => None,
//             // Type::Primitive(p) => p.as_basic_metadata_type_enum(),
//             Type::Struct(s) => s.as_basic_metadata_type_enum(),
//         }
//     }

//     pub const fn as_basic_type_enum(&self) -> Option<BasicTypeEnum<'static>> {
//         match self {
//             Type::I32(i) => Some(BasicTypeEnum::IntType(i.0)),
//             Type::Void(_) => None,
//             // Type::Primitive(p) => p.as_basic_type_enum(),
//             Type::Struct(s) => s.as_basic_type_enum(),
//         }
//     }
// }

// //

// #[derive(Debug, Clone, Copy)]
// pub struct I32(IntType<'static>);

// #[derive(Debug, Clone, Copy)]
// pub struct I32Value(I32, IntValue<'static>);

// impl I32 {
//     pub fn ty() -> Self {
//         Self(context().i32_type())
//     }

//     pub fn val(v: i32) -> I32Value {
//         let ty = Self::ty();
//         I32Value(ty, ty.0.const_int(v as u64, false))
//     }
// }

// impl From<I32> for Type {
//     fn from(value: I32) -> Self {
//         Type::I32(value)
//     }
// }

// impl From<I32Value> for Value {
//     fn from(value: I32Value) -> Self {
//         Value::I32(value)
//     }
// }

// //

// #[derive(Debug, Clone, Copy)]
// pub struct Void;

// #[derive(Debug, Clone, Copy)]
// pub struct VoidValue(Void);

// impl Void {
//     pub fn ty() -> Self {
//         Self
//     }

//     pub fn val() -> VoidValue {
//         VoidValue(Void)
//     }
// }

// impl From<Void> for Type {
//     fn from(value: Void) -> Self {
//         Type::Void(value)
//     }
// }

// impl From<VoidValue> for Value {
//     fn from(value: VoidValue) -> Self {
//         Value::Void(value)
//     }
// }

// //

// #[test]
// fn test() {
//     let mut types = Types::new();

//     let point = types
//         .new_struct("Point")
//         // .align(16)
//         .field("x", I32::ty())
//         .field("y", Void::ty())
//         .build();

//     point.size();

//     let v = point
//         .instance()
//         .field("x", I32::val(32))
//         // .field("y", I32::val(32))
//         .field("y", Void::val())
//         .build();

//     let grid = types
//         .new_struct("Point")
//         // .align(16)
//         .field("a", point.clone())
//         .field("b", point)
//         .build();

//     grid.instance().field("a", v.clone()).field("b", v).build();

//     // let _option = types.new_enum().align(16).variant("").build();
// }
