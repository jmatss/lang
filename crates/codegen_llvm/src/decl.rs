use inkwell::module::Linkage;

use analyze::util::order::dependency_order;
use common::{
    error::LangResult,
    token::{
        ast::AstToken,
        block::{AdtKind, Block, BlockHeader},
        stmt::Modifier,
    },
};

use crate::generator::CodeGen;

impl<'a, 'b, 'ctx> CodeGen<'a, 'b, 'ctx> {
    /// Compile all declarations of types: structs, enums, unions and interfaces.
    /// This will be done at the start of the code generation so that one
    /// doesn't have do declare prototypes manual in the source before the use
    /// of the type.
    /// This function shall be ran before the function/method prototypes
    /// are compiled since they might contains references to types.
    pub(super) fn compile_type_decl(&mut self, ast_token: &mut AstToken) -> LangResult<()> {
        // TODO: Currently only returns the first error, should return all.
        let include_impls = false;
        let full_paths = true;
        let order = dependency_order(self.analyze_ctx, ast_token, include_impls, full_paths)
            .map_err(|e| e.first().cloned().unwrap())?;

        for adt_path in &order {
            let adt = self
                .analyze_ctx
                .ast_ctx
                .get_adt(&self.analyze_ctx.ty_env.lock(), adt_path)?;
            let adt = adt.read();

            match adt.kind {
                AdtKind::Struct => {
                    self.compile_struct_decl(&adt)?;
                }
                AdtKind::Enum => {
                    self.compile_enum_decl(&adt)?;
                }
                AdtKind::Union => {
                    self.compile_union_decl(&adt)?;
                }
                AdtKind::Unknown => unreachable!("Tried to compile AdtKind::Unknown"),
            }
        }

        Ok(())
    }

    /// Compile all declarations of functions and methods (implement blocks).
    /// This will be done at the start of the code generation so that one doesn't
    /// have do declare prototypes manual in the source before the use of the
    /// function/method.
    pub(super) fn compile_fn_decl(&mut self, mut ast_token: &mut AstToken) -> LangResult<()> {
        if let AstToken::Block(Block {
            header,
            body,
            id,
            file_pos,
        }) = &mut ast_token
        {
            self.cur_block_id = *id;

            if let BlockHeader::Fn(func) = header {
                let func = func.read();

                let linkage = if func.modifiers.contains(&Modifier::Public)
                    || (func.name == "main" && func.module.count() == 0)
                {
                    Linkage::External
                } else {
                    // Private and Hidden.
                    // TODO: Linkage::Private
                    ();
                    Linkage::External
                };
                self.compile_fn_proto(&func, Some(file_pos.to_owned()), Some(linkage))?;
            }

            for token in body {
                self.compile_fn_decl(token)?
            }
        }

        Ok(())
    }
}
