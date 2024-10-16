use core::fmt;
use std::{slice, str};

use inkwell::{
    context::Context,
    types::{BasicMetadataTypeEnum, BasicTypeEnum, FunctionType, StructType},
    values::{BasicValue, BasicValueEnum, StructValue},
    AddressSpace,
};
use typeck::{Literal, Type};

use crate::ModuleGen;

//

pub trait AsType {
    const TYPE_ID: Type;
}

impl AsType for bool {
    const TYPE_ID: Type = Type::Bool;
}

impl AsType for i32 {
    const TYPE_ID: Type = Type::I32;
}

impl AsType for () {
    const TYPE_ID: Type = Type::Void;
}

//

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

    pub fn get_type(gen: &ModuleGen) -> StructType<'static> {
        get_or_init_struct(gen.ctx, "str", |s| {
            s.set_body(
                &[
                    gen.ctx
                        .ptr_sized_int_type(gen.engine.get_target_data(), None)
                        .into(),
                    gen.ctx.i8_type().ptr_type(AddressSpace::default()).into(),
                ],
                false,
            );
        })
    }

    pub fn get_const(gen: &ModuleGen, str: &str) -> StructValue<'static> {
        let str_len = gen
            .ctx
            .ptr_sized_int_type(gen.engine.get_target_data(), None)
            .const_int(str.len() as _, false)
            .as_basic_value_enum();
        let str_ptr = gen
            .builder
            .build_global_string_ptr(str, str)
            .unwrap()
            .as_basic_value_enum();

        Self::get_type(gen).const_named_struct(&[str_len, str_ptr])
    }
}

impl fmt::Display for Str {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl AsType for Str {
    const TYPE_ID: Type = Type::Str;
}

//

pub trait FnAsLlvm {
    fn return_type(&self) -> Type;

    fn params(&self) -> &[Type];

    fn as_extern_c_fn_ptr(&self) -> usize;
}

impl<R: AsType> FnAsLlvm for extern "C" fn() -> R {
    fn return_type(&self) -> Type {
        R::TYPE_ID
    }

    fn params(&self) -> &[Type] {
        &[]
    }

    fn as_extern_c_fn_ptr(&self) -> usize {
        *self as usize
    }
}

impl<A: AsType, R: AsType> FnAsLlvm for extern "C" fn(A) -> R {
    fn return_type(&self) -> Type {
        R::TYPE_ID
    }

    fn params(&self) -> &[Type] {
        &[A::TYPE_ID]
    }

    fn as_extern_c_fn_ptr(&self) -> usize {
        *self as usize
    }
}

impl<A: AsType, B: AsType, R: AsType> FnAsLlvm for extern "C" fn(A, B) -> R {
    fn return_type(&self) -> Type {
        R::TYPE_ID
    }

    fn params(&self) -> &[Type] {
        &[A::TYPE_ID, B::TYPE_ID]
    }

    fn as_extern_c_fn_ptr(&self) -> usize {
        *self as usize
    }
}

//

pub trait AsLlvmConst {
    fn as_llvm_const(&self, gen: &ModuleGen) -> Option<BasicValueEnum<'static>>;
}

impl AsLlvmConst for Literal {
    fn as_llvm_const(&self, gen: &ModuleGen) -> Option<BasicValueEnum<'static>> {
        match self {
            Literal::Bool(v) => Some(gen.ctx.bool_type().const_int(*v as u64, false).into()),
            Literal::I32(v) => Some(gen.ctx.i32_type().const_int(*v as u64, false).into()),
            Literal::Str(v) => Some(Str::get_const(gen, &v).into()),
        }
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

impl AsLlvm for Type {
    fn as_llvm_fn(
        &self,
        gen: &ModuleGen,
        param_types: &[BasicMetadataTypeEnum<'static>],
        is_var_args: bool,
    ) -> FunctionType<'static> {
        let ctx = gen.ctx;
        match self {
            Type::Bool => ctx.bool_type().fn_type(param_types, is_var_args),
            Type::I32 => ctx.i32_type().fn_type(param_types, is_var_args),
            Type::Str => Str::get_type(gen).fn_type(param_types, is_var_args),
            Type::Void => ctx.void_type().fn_type(param_types, is_var_args),
            Type::Never => ctx.void_type().fn_type(param_types, is_var_args),
            Type::Func(_f) => ctx.void_type().fn_type(param_types, is_var_args),
            Type::Unknown => todo!(),
        }
    }

    fn as_llvm(&self, gen: &ModuleGen) -> Option<BasicTypeEnum<'static>> {
        let ctx = gen.ctx;
        match self {
            Type::Bool => Some(ctx.bool_type().into()),
            Type::I32 => Some(ctx.i32_type().into()),
            Type::Str => Some(Str::get_type(gen).into()),
            Type::Void => None,
            Type::Never => None,
            Type::Func(_func_id) => None, // Some(get_or_init_struct(ctx, &format!("[anon_func_{}]", func_id.0)).into()),
            Type::Unknown => todo!(),
        }
    }

    fn as_llvm_meta(&self, gen: &ModuleGen) -> Option<BasicMetadataTypeEnum<'static>> {
        let ctx = gen.ctx;
        match self {
            Type::Bool => Some(ctx.bool_type().into()),
            Type::I32 => Some(ctx.i32_type().into()),
            Type::Str => Some(Str::get_type(gen).into()),
            Type::Void => None,
            Type::Never => None,
            Type::Func(_func_id) => None, // Some(get_or_init_struct(ctx, &format!("[anon_func_{}]", func_id.0)).into()),
            Type::Unknown => todo!(),
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
