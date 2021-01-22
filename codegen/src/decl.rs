use crate::generator::CodeGen;
use analyze::{block::BlockInfo, util::order::dependency_order};
use common::{
    error::CustomResult,
    token::{ast::AstToken, block::BlockHeader},
};
use inkwell::module::Linkage;

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    /// Compile all declarations of types: structs, enums and interfaces.
    /// This will be done at the start of the code generation so that one
    /// doesn't have do declare prototypes manual in the source before the use
    /// of the type.
    /// This function shall be ran before the function/method prototypes
    /// are compiled since they might contains references to types.
    pub(super) fn compile_type_decl(&mut self, ast_token: &mut AstToken) -> CustomResult<()> {
        self.cur_file_pos = ast_token.file_pos().cloned().unwrap_or_default();

        // TODO:
        let block_id = BlockInfo::DEFAULT_BLOCK_ID;

        // TODO: Currently only returns the first error, should return all.
        let include_impls = false;
        let full_names = true;
        let order = dependency_order(ast_token, include_impls, full_names)
            .map_err(|e| e.first().cloned().unwrap())?;

        for ident in &order {
            if let Ok(struct_) = self.analyze_context.get_struct(ident, block_id) {
                self.compile_struct(&struct_.borrow())?;
            } else if let Ok(enum_) = self.analyze_context.get_enum(ident, block_id) {
                self.compile_enum(&enum_.borrow())?;
            } else {
                return Err(self.err(
                    format!(
                        "Unable to find structure with name \"{}\" when compiling type decl.",
                        ident
                    ),
                    None,
                ));
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

        if let AstToken::Block(header, file_pos, id, ref mut body) = &mut ast_token {
            self.cur_block_id = *id;

            match header {
                BlockHeader::Function(func) => {
                    let linkage = Linkage::External;
                    self.compile_func_proto(
                        &func.borrow(),
                        Some(file_pos.to_owned()),
                        Some(linkage),
                    )?;
                }
                BlockHeader::Implement(..) => {
                    for mut ast_token in body.iter_mut() {
                        if let AstToken::Block(BlockHeader::Function(func), ..) = &mut ast_token {
                            let linkage = Linkage::External;
                            self.compile_func_proto(
                                &func.borrow(),
                                Some(file_pos.to_owned()),
                                Some(linkage),
                            )?;
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
}
