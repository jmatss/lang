use common::error::LangResult;

use ir::{EndInstr, Val};

use crate::{generator::CodeGen, util::any_into_basic_value};

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    pub(super) fn compile_end_instr(&mut self, end_instr: &EndInstr) -> LangResult<()> {
        match end_instr {
            EndInstr::Return(val_opt) => self.compile_return(val_opt.as_ref()),
            EndInstr::Exit => todo!("compile_end_instr -- exit"),
            EndInstr::Branch(label) => self.compile_branch(label),
            EndInstr::BranchIf(val, label_true, label_false) => {
                self.compile_branch_if(val, label_true, label_false)
            }
            EndInstr::BranchSwitch(val, label_default, cases) => {
                self.compile_branch_switch(val, label_default, cases)
            }
            EndInstr::Unreachable => self.compile_unreachable(),
        }
    }

    fn compile_return(&mut self, val_opt: Option<&Val>) -> LangResult<()> {
        if let Some(val) = val_opt {
            let any_value = self.get_compiled_val(val)?;
            let basic_value = any_into_basic_value(any_value)?;
            self.builder.build_return(Some(&basic_value));
        } else {
            self.builder.build_return(None);
        }
        Ok(())
    }

    fn compile_branch(&mut self, label: &str) -> LangResult<()> {
        if let Some(compiled_block) = self.compiled_blocks.get(label) {
            self.builder.build_unconditional_branch(*compiled_block);
            Ok(())
        } else {
            Err(self.err(
                format!(
                    "Unable to find branch with label \"{}\" when compiling branch.",
                    label
                ),
                None,
            ))
        }
    }

    fn compile_branch_if(
        &mut self,
        val: &Val,
        label_true: &str,
        label_false: &str,
    ) -> LangResult<()> {
        let block_true = if let Some(block) = self.compiled_blocks.get(label_true) {
            *block
        } else {
            return Err(self.err(
                format!(
                    "Unable to find branch with label \"{}\" when compiling branch if true.",
                    label_true
                ),
                None,
            ));
        };

        let block_false = if let Some(block) = self.compiled_blocks.get(label_false) {
            *block
        } else {
            return Err(self.err(
                format!(
                    "Unable to find branch with label \"{}\" when compiling branch if false.",
                    label_false
                ),
                None,
            ));
        };

        let any_value = self.get_compiled_val(val)?;

        if any_value.is_int_value() {
            self.builder.build_conditional_branch(
                any_value.into_int_value(),
                block_true,
                block_false,
            );
            Ok(())
        } else {
            Err(self.err(format!("Value in branch if not int, was: {:?}", val), None))
        }
    }

    fn compile_branch_switch(
        &mut self,
        val: &Val,
        label_default: &str,
        cases: &[(Val, String)],
    ) -> LangResult<()> {
        let block_default = if let Some(block) = self.compiled_blocks.get(label_default) {
            *block
        } else {
            return Err(self.err(
                format!(
                    "Unable to find branch with label \"{}\" when compiling branch switch default.",
                    label_default
                ),
                None,
            ));
        };

        let mut compiled_cases = Vec::with_capacity(cases.len());
        for (val_case, label_case) in cases {
            let case_any_value = self.get_compiled_val(val_case)?;
            if !case_any_value.is_int_value() {
                return Err(self.err(
                    format!("Value in branch switch case not int, was: {:?}", val),
                    None,
                ));
            }

            let block_case = if let Some(block) = self.compiled_blocks.get(label_case) {
                *block
            } else {
                return Err(self.err(
                    format!(
                        "Unable to find branch with label \"{}\" when compiling branch switch case.",
                        label_case
                    ),
                    None,
                ));
            };

            compiled_cases.push((case_any_value.into_int_value(), block_case));
        }

        let any_value = self.get_compiled_val(val)?;

        if any_value.is_int_value() && any_value.into_int_value().is_constant_int() {
            self.builder
                .build_switch(any_value.into_int_value(), block_default, &compiled_cases);
            Ok(())
        } else {
            Err(self.err(
                format!("Value in branch switch not constant int, was: {:?}", val),
                None,
            ))
        }
    }

    fn compile_unreachable(&mut self) -> LangResult<()> {
        self.builder.build_unreachable();
        Ok(())
    }
}
