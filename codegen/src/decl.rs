use crate::generator::CodeGen;
use analyze::{block::BlockInfo, util::order::dependency_order};
use common::{
    error::LangResult,
    token::{
        ast::AstToken,
        block::{AdtKind, BlockHeader},
        stmt::Modifier,
    },
};
use inkwell::module::Linkage;

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    /// Compile all declarations of types: structs, enums, unions and interfaces.
    /// This will be done at the start of the code generation so that one
    /// doesn't have do declare prototypes manual in the source before the use
    /// of the type.
    /// This function shall be ran before the function/method prototypes
    /// are compiled since they might contains references to types.
    pub(super) fn compile_type_decl(&mut self, ast_token: &mut AstToken) -> LangResult<()> {
        self.cur_file_pos = ast_token.file_pos().cloned().unwrap_or_default();

        // TODO:
        let block_id = BlockInfo::DEFAULT_BLOCK_ID;

        // TODO: Currently only returns the first error, should return all.
        let include_impls = false;
        let full_paths = true;
        let order = dependency_order(self.analyze_context, ast_token, include_impls, full_paths)
            .map_err(|e| e.first().cloned().unwrap())?;

        for ident in &order {
            let adt = self.analyze_context.get_adt(ident, block_id)?;
            let adt = adt.borrow();

            match adt.kind {
                AdtKind::Struct => {
                    self.compile_struct(&adt)?;
                }
                AdtKind::Enum => {
                    self.compile_enum(&adt)?;
                }
                AdtKind::Union => {
                    self.compile_union(&adt)?;
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
        self.cur_file_pos = ast_token.file_pos().cloned().unwrap_or_default();

        if let AstToken::Block(header, file_pos, id, ref mut body) = &mut ast_token {
            self.cur_block_id = *id;

            if let BlockHeader::Fn(func) = header {
                let func = func.borrow();
                let linkage = if func.name == "main" || func.modifiers.contains(&Modifier::Public) {
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
