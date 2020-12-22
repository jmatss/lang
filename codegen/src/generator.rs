use analyze::AnalyzeContext;
use common::{
    error::{CustomResult, LangError, LangErrorKind::CodeGenError},
    file::FilePosition,
    token::block::BlockHeader,
    token::{
        ast::AstToken,
        expr::{Expr, Var},
        lit::Lit,
    },
    ty::{inner_ty::InnerTy, ty::Ty},
    util, BlockId,
};
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Linkage,
    module::Module,
    targets::TargetMachine,
    types::{AnyTypeEnum, BasicTypeEnum},
    values::{AnyValueEnum, BasicValueEnum, FunctionValue, InstructionValue, PointerValue},
    AddressSpace,
};
use log::debug;
use std::collections::HashMap;

use crate::expr::ExprTy;

pub(super) struct CodeGen<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,
    pub target_machine: &'a TargetMachine,

    /// Information parsed during the "Analyzing" stage. This contains ex.
    /// defintions (var, struct, func etc.) and information about the AST blocks.
    pub analyze_context: &'ctx AnalyzeContext,

    /// The ID of the current block that is being compiled.
    pub cur_block_id: BlockId,

    /// The file position of the current token.
    pub cur_file_pos: FilePosition,

    /// Contains the current basic block that instructions are inserted into.
    pub cur_basic_block: Option<BasicBlock<'ctx>>,

    /// Contains a pointer to the current function that is being generated.
    pub cur_func: Option<FunctionValue<'ctx>>,

    /// Contains the current "branch block" if the current block has one. This is
    /// true for "while" and "for" blocks. This branch block will then be
    /// used when a continue call is done to find the start of the loop.
    pub cur_branch_block: Option<BasicBlock<'ctx>>,

    /// Contains the latest compiled expression. This will be used when compiling
    /// unary operations for expressions. This allows the cur expression that is
    /// being compiled to know about the previous expressions which will allow
    /// for chainining operations.
    pub prev_expr: Option<AnyValueEnum<'ctx>>,

    // TODO: Remove this weird variable. Its only purpose is to be used if a
    //       deref is found in the lhs of a assignment. Then the pointer to the
    //       derefed value will be stored in this variable. Do this some other
    //       way. It gets assigned when compiling a un op deref and read when
    //       compiling a assignment.
    /// Contains the pointer of the last dereferenced expression.
    /// This will be used for expressions in the lhs of a assignment. In those
    /// cases one doesn't want the value of the deref, one wannts the pointer
    /// to the value.
    pub prev_deref_ptr: Option<PointerValue<'ctx>>,

    /// Merge blocks created for different if and match statements.
    /// Is stored in this struct so that it can be accessable from everywhere
    /// and statements etc. can figure out where to branch.
    pub merge_blocks: HashMap<BlockId, BasicBlock<'ctx>>,

    /// Contains pointers to mutable variables that have been compiled.
    pub variables: HashMap<(String, BlockId), PointerValue<'ctx>>,

    /// Contains constant variables. They can't be used as regular variable
    /// in the code. Keep track of them in this hashmap and do calculations
    /// and update them in here during the codegen process.
    pub constants: HashMap<(String, BlockId), BasicValueEnum<'ctx>>,
}

pub fn generate<'a, 'ctx>(
    ast_root: &'ctx mut AstToken,
    analyze_context: &'ctx AnalyzeContext,
    context: &'ctx Context,
    builder: &'a Builder<'ctx>,
    module: &'a Module<'ctx>,
    target_machine: &'a TargetMachine,
) -> CustomResult<()> {
    let mut code_gen = CodeGen::new(context, analyze_context, builder, module, target_machine);
    // Start by first compiling all types (structs/enums/inferfaces) and after
    // that all functions/methods. This makes it so that one doesn't have to
    // specifiy type/func prototypes above their use in the source code.
    code_gen.compile_type_decl(ast_root)?;
    code_gen.compile_func_decl(ast_root)?;
    code_gen.compile(ast_root)?;

    // TODO: Temporary solution, loop through all merge blocks and look for all
    //       merge blocks with no terminator instruction. If the merge block has
    //       a "wrapping" block, the merge block should branch to the wrapping
    //       blocks merge block. Otherwise something has gone wrong.
    for (block_id, merge_block) in &code_gen.merge_blocks {
        if merge_block.get_terminator().is_none() {
            let parent_block_id = code_gen
                .analyze_context
                .block_info
                .get(&block_id)
                .ok_or_else(|| {
                    LangError::new(
                        format!("Unable to find block info for block with id {}", block_id),
                        CodeGenError {
                            file_pos: FilePosition::default(),
                        },
                    )
                })?
                .parent_id;

            if let Ok(wrapping_merge_block) = code_gen.get_merge_block(parent_block_id) {
                code_gen.builder.position_at_end(*merge_block);
                code_gen
                    .builder
                    .build_unconditional_branch(wrapping_merge_block);
            } else {
                return Err(code_gen.err(format!(
                    "MergeBlock for block with ID {} has no terminator and no wrapping block.",
                    block_id
                )));
            }
        }
    }
    Ok(())
}

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    fn new(
        context: &'ctx Context,
        analyze_context: &'ctx AnalyzeContext,
        builder: &'a Builder<'ctx>,
        module: &'a Module<'ctx>,
        target_machine: &'a TargetMachine,
    ) -> Self {
        Self {
            context,
            builder,
            module,
            target_machine,

            analyze_context,

            cur_file_pos: FilePosition::default(),

            cur_block_id: 0,
            cur_basic_block: None,
            cur_func: None,
            cur_branch_block: None,

            prev_expr: None,
            prev_deref_ptr: None,

            merge_blocks: HashMap::default(),
            variables: HashMap::default(),
            constants: HashMap::default(),
        }
    }

    /// Compile all declarations of types: structs, enums and interfaces.
    /// This will be done at the start of the code generation so that one
    /// doesn't have do declare prototypes manual in the source before the use
    /// of the type.
    /// This function shall be ran before the function/method prototypes
    /// are compiled since they might contains references to types.
    pub(super) fn compile_type_decl(&mut self, mut ast_token: &mut AstToken) -> CustomResult<()> {
        self.cur_file_pos = ast_token.file_pos().cloned().unwrap_or_default();

        if let AstToken::Block(header, id, ref mut body) = &mut ast_token {
            self.cur_block_id = *id;

            match header {
                BlockHeader::Struct(struct_) => {
                    self.compile_struct(&struct_.borrow())?;
                }
                BlockHeader::Enum(enum_) => {
                    panic!("TODO: Enum");
                    //self.compile_enum(enum_);
                }
                BlockHeader::Interface(interface) => {
                    panic!("TODO: interface");
                    //self.compile_interface(interface);
                }
                _ => (),
            }

            for token in body {
                self.compile_type_decl(token)?
            }
        }

        Ok(())
    }

    /// Compile all declarations of functions and methods (implement blocks).
    /// This will be done at the start of the code generation so that one doesn't
    /// have do declare prototypes manual in the source before the use of the
    /// function/method.
    pub(super) fn compile_func_decl(&mut self, mut ast_token: &mut AstToken) -> CustomResult<()> {
        self.cur_file_pos = ast_token.file_pos().cloned().unwrap_or_default();

        if let AstToken::Block(header, id, ref mut body) = &mut ast_token {
            self.cur_block_id = *id;

            match header {
                BlockHeader::Function(func) => {
                    let linkage = Linkage::External;
                    self.compile_func_proto(&func.borrow(), Some(linkage))?;
                }
                BlockHeader::Implement(_) => {
                    for mut ast_token in body.iter_mut() {
                        if let AstToken::Block(BlockHeader::Function(func), ..) = &mut ast_token {
                            let linkage = Linkage::External;
                            self.compile_func_proto(&func.borrow(), Some(linkage))?;
                        }
                    }
                }
                _ => (),
            }

            for token in body {
                self.compile_func_decl(token)?
            }
        }

        Ok(())
    }

    pub(super) fn compile(&mut self, mut ast_token: &mut AstToken) -> CustomResult<()> {
        self.cur_file_pos = ast_token.file_pos().cloned().unwrap_or_default();

        match &mut ast_token {
            AstToken::Block(header, id, ref mut body) => {
                self.compile_block(header, *id, body)?;
            }
            AstToken::Stmt(ref mut stmt) => {
                self.compile_stmt(stmt)?;
            }
            AstToken::Expr(ref mut expr) => {
                self.compile_expr(expr, ExprTy::RValue)?;
            }
            AstToken::Empty | AstToken::EOF => (),
        }
        Ok(())
    }

    pub(super) fn alloca_var(&self, var: &Var) -> CustomResult<PointerValue<'ctx>> {
        if let Some(var_type) = &var.ty {
            Ok(match self.compile_type(&var_type)? {
                AnyTypeEnum::ArrayType(ty) => {
                    let sign_extend = false;
                    let dim = self
                        .context
                        .i64_type()
                        .const_int(ty.len() as u64, sign_extend);
                    self.builder.build_array_alloca(ty, dim, &var.name)
                }
                AnyTypeEnum::FloatType(ty) => self.builder.build_alloca(ty, &var.name),
                AnyTypeEnum::IntType(ty) => self.builder.build_alloca(ty, &var.name),
                AnyTypeEnum::PointerType(ty) => self.builder.build_alloca(ty, &var.name),
                AnyTypeEnum::StructType(ty) => self.builder.build_alloca(ty, &var.name),
                AnyTypeEnum::VectorType(ty) => self.builder.build_alloca(ty, &var.name),
                AnyTypeEnum::FunctionType(_) => {
                    return Err(self.err("Tried to alloca function.".into()));
                }
                AnyTypeEnum::VoidType(_) => {
                    return Err(self.err("Tried to alloca void type.".into()));
                }
            })
        } else {
            Err(self.err(format!("type None when allocating var: {:?}", &var.name)))
        }
    }

    pub(super) fn compile_var_decl(&mut self, var: &Var) -> CustomResult<()> {
        let decl_block_id = self
            .analyze_context
            .get_var_decl_scope(&var.name, self.cur_block_id)?;
        let key = (var.name.clone(), decl_block_id);

        match self.analyze_context.get_var(&var.name, self.cur_block_id) {
            Ok(var_decl) => {
                debug!("Compiling var var_decl: {:#?}", &var_decl);

                // Constants are never "compiled" into instructions, they are handled
                // "internaly" in this code during compilation.
                if !var.is_const {
                    let ptr = self.alloca_var(&var_decl.borrow())?;
                    self.variables.insert(key, ptr);
                }

                Ok(())
            }
            Err(err) => Err(err),
        }
    }

    pub(super) fn compile_var_store(
        &mut self,
        var: &Var,
        basic_value: BasicValueEnum<'ctx>,
    ) -> CustomResult<InstructionValue<'ctx>> {
        debug!(
            "Compile var_store, var name: {:?}\nret_type: {:#?}\nbasic_value: {:#?}.",
            &var.name, &var.ty, &basic_value
        );

        // TODO: Const isn't working atm.
        if var.is_const {
            let block_id = self.cur_block_id;
            let decl_block_id = self
                .analyze_context
                .get_var_decl_scope(&var.name, block_id)?;
            let key = (var.name.clone(), decl_block_id);

            self.constants.insert(key, basic_value);
        }

        let ptr = self.get_var_ptr(var)?;
        debug!("ptr value: {:?}", ptr);

        Ok(self.builder.build_store(ptr, basic_value))
    }

    pub(super) fn compile_var_load(&mut self, var: &Var) -> CustomResult<BasicValueEnum<'ctx>> {
        if var.is_const {
            self.get_const_value(var)
        } else {
            let ptr = self.get_var_ptr(var)?;
            Ok(self.builder.build_load(ptr, "load"))
        }
    }

    // TODO: Implement logic to load both regular variables and struct members
    //       if they are const.
    fn get_const_value(&mut self, var: &Var) -> CustomResult<BasicValueEnum<'ctx>> {
        let block_id = self.cur_block_id;
        let decl_block_id = self
            .analyze_context
            .get_var_decl_scope(&var.name, block_id)?;
        let key = (var.name.clone(), decl_block_id);
        debug!("Loading constant pointer. Key: {:?}", &key);

        if let Some(const_value) = self.constants.get(&key) {
            Ok(*const_value)
        } else {
            Err(self.err(format!(
                "Unable to find value for constant \"{}\" in decl block ID {}.",
                &var.name, decl_block_id
            )))
        }
    }

    pub(crate) fn get_var_ptr(&mut self, var: &Var) -> CustomResult<PointerValue<'ctx>> {
        let block_id = self.cur_block_id;
        let decl_block_id = self
            .analyze_context
            .get_var_decl_scope(&var.name, block_id)?;
        let key = (var.name.clone(), decl_block_id);
        debug!(
            "Loading variable pointer. Key: {:?}, cur_block_id: {}",
            &key, block_id
        );

        if let Some(var_ptr) = self.variables.get(&key) {
            Ok(*var_ptr)
        } else {
            Err(self.err(format!(
                "Unable to find ptr for variable \"{}\" in decl block ID {}.",
                &var.name, decl_block_id
            )))
        }
    }

    pub(super) fn compile_type(&self, ty: &Ty) -> CustomResult<AnyTypeEnum<'ctx>> {
        // TODO: What AddressSpace should be used?
        let address_space = AddressSpace::Generic;

        Ok(match ty {
            Ty::Pointer(ref ptr) => {
                // Get the type of the inner type and wrap into a "PointerType".
                match self.compile_type(ptr)? {
                    AnyTypeEnum::ArrayType(ty) => ty.ptr_type(address_space).into(),
                    AnyTypeEnum::FloatType(ty) => ty.ptr_type(address_space).into(),
                    AnyTypeEnum::FunctionType(ty) => ty.ptr_type(address_space).into(),
                    AnyTypeEnum::IntType(ty) => ty.ptr_type(address_space).into(),
                    AnyTypeEnum::PointerType(ty) => ty.ptr_type(address_space).into(),
                    AnyTypeEnum::StructType(ty) => ty.ptr_type(address_space).into(),
                    AnyTypeEnum::VectorType(ty) => ty.ptr_type(address_space).into(),
                    AnyTypeEnum::VoidType(_) => {
                        // TODO: FIXME: Is this OK? Can't use pointer to void, use
                        //              pointer to a generic I8 instead.
                        self.context.i8_type().ptr_type(address_space).into()
                    }
                }
            }

            // TODO: Calculate array size that contains ther things than just
            //       a single integer literal
            Ty::Array(inner_ty, dim_opt) => {
                let lit_dim = if let Some(dim) = dim_opt {
                    match dim.as_ref() {
                        Expr::Lit(lit, ..) => match lit {
                            Lit::Integer(num, radix) => u32::from_str_radix(num, *radix)?,
                            _ => {
                                return Err(self.err(format!(
                                    "Invalid literal used as array dimension: {:?}",
                                    lit
                                )))
                            }
                        },
                        _ => {
                            return Err(self.err(format!(
                                "TODO: Invalid expression used as array dimension: {:?}",
                                dim
                            )))
                        }
                    }
                } else {
                    return Err(self.err("No dimension set for array.".into()));
                };

                match self.compile_type(inner_ty)? {
                    AnyTypeEnum::ArrayType(ty) => ty.array_type(lit_dim).into(),
                    AnyTypeEnum::FloatType(ty) => ty.array_type(lit_dim).into(),
                    AnyTypeEnum::IntType(ty) => ty.array_type(lit_dim).into(),
                    AnyTypeEnum::PointerType(ty) => ty.array_type(lit_dim).into(),
                    AnyTypeEnum::StructType(ty) => ty.array_type(lit_dim).into(),
                    AnyTypeEnum::VectorType(ty) => ty.array_type(lit_dim).into(),
                    AnyTypeEnum::FunctionType(_) => {
                        return Err(self.err("Tried to array index into function type.".into()))
                    }
                    AnyTypeEnum::VoidType(_) => {
                        return Err(self.err("Tried to array index into void type.".into()))
                    }
                }
            }

            // TODO: Implement for other types (enum/interface) as well.
            Ty::CompoundType(inner_ty, generics) => {
                match inner_ty {
                    InnerTy::Struct(ident) => {
                        let ident = if !generics.is_empty() {
                            util::to_generic_struct_name(ident, generics)
                        } else {
                            ident.clone()
                        };

                        if let Some(struct_type) = self.module.get_struct_type(&ident) {
                            struct_type.clone().into()
                        } else {
                            return Err(self.err(format!(
                                "Unable to find custom compound type with name: {:#?}",
                                ident
                            )));
                        }
                    }
                    InnerTy::Enum(_) => {
                        panic!("TODO: Enum")
                    }
                    InnerTy::Interface(_) => {
                        panic!("TODO: interface")
                    }
                    InnerTy::Void => AnyTypeEnum::VoidType(self.context.void_type()),
                    InnerTy::Character => AnyTypeEnum::IntType(self.context.i32_type()),
                    // TODO: What type should the string be?
                    InnerTy::String => {
                        AnyTypeEnum::PointerType(self.context.i8_type().ptr_type(address_space))
                    }
                    InnerTy::Boolean => AnyTypeEnum::IntType(self.context.bool_type()),
                    InnerTy::I8 => AnyTypeEnum::IntType(self.context.i8_type()),
                    InnerTy::U8 => AnyTypeEnum::IntType(self.context.i8_type()),
                    InnerTy::I16 => AnyTypeEnum::IntType(self.context.i16_type()),
                    InnerTy::U16 => AnyTypeEnum::IntType(self.context.i16_type()),
                    InnerTy::I32 => AnyTypeEnum::IntType(self.context.i32_type()),
                    InnerTy::U32 => AnyTypeEnum::IntType(self.context.i32_type()),
                    InnerTy::F32 => AnyTypeEnum::FloatType(self.context.f32_type()),
                    InnerTy::I64 => AnyTypeEnum::IntType(self.context.i64_type()),
                    InnerTy::U64 => AnyTypeEnum::IntType(self.context.i64_type()),
                    InnerTy::F64 => AnyTypeEnum::FloatType(self.context.f64_type()),
                    InnerTy::I128 => AnyTypeEnum::IntType(self.context.i128_type()),
                    InnerTy::U128 => AnyTypeEnum::IntType(self.context.i128_type()),

                    _ => {
                        return Err(self.err(format!("Invalid type during type codegen: {:?}", ty)))
                    }
                }
            }

            _ => return Err(self.err(format!("Invalid type during type codegen: {:?}", ty))),
        })
    }

    /// Returns true if the types have the same "base type". Ex. if both values
    /// are int, float, pointer etc.
    pub(crate) fn is_same_base_type(
        &self,
        left_type: BasicTypeEnum<'ctx>,
        right_type: BasicTypeEnum<'ctx>,
    ) -> bool {
        left_type.is_int_type() && right_type.is_int_type()
            || left_type.is_float_type() && right_type.is_float_type()
            || left_type.is_array_type() && right_type.is_array_type()
            || left_type.is_pointer_type() && right_type.is_pointer_type()
            || left_type.is_struct_type() && right_type.is_struct_type()
            || left_type.is_vector_type() && right_type.is_vector_type()
    }

    /// Used when returing errors to include current line/column number.
    pub fn err(&self, msg: String) -> LangError {
        LangError::new_backtrace(
            msg,
            CodeGenError {
                file_pos: self.cur_file_pos,
            },
            true,
        )
    }
}
