use std::fmt::Debug;

use crate::{
    error::{IrError, IrResult},
    instr::{EndInstr, ExprInstr},
};

pub struct BasicBlock {
    /// The name/label of this basic block.
    pub label: String,

    /// A list of all instructions inside the body of this basic block (excluding
    /// the last `end_instruction`).
    ///
    /// The order of the instructions inside the `instructions` vector is the
    /// "actual" order of the instructions and the flow of the program.
    instrs: Vec<ExprInstr>,

    /// The last instruction of this basic block. This should some sort of
    /// exit/branch instruction.
    end_instrs: Option<EndInstr>,
}

impl BasicBlock {
    pub fn new(label: String) -> Self {
        Self {
            label,
            instrs: Vec::default(),
            end_instrs: None,
        }
    }

    pub fn get_end_instr(&mut self) -> Option<&EndInstr> {
        self.end_instrs.as_ref()
    }

    pub fn set_end_instr(&mut self, end_instr: EndInstr) {
        self.end_instrs = Some(end_instr);
    }

    pub fn has_end_instr(&self) -> bool {
        self.end_instrs.is_some()
    }

    /// Adds a instruction to the end of this basic block (EXCLUDING end instruction).
    pub fn push(&mut self, instr: ExprInstr) {
        self.instrs.push(instr);
    }

    /// Returns the instruction with index `idx`.
    pub fn get(&self, idx: usize) -> Option<&ExprInstr> {
        self.instrs.get(idx)
    }

    /// Returns the first instruction of this basic block.
    pub fn first(&self) -> Option<&ExprInstr> {
        self.instrs.first()
    }

    /// Returns the last instruction of this basic block (EXCLUDING end instruction).
    pub fn last(&self) -> Option<&ExprInstr> {
        self.instrs.last()
    }

    /// Removes the instruction at index `idx`, shifting the remanining
    /// instructions to the "left".
    pub fn remove(&mut self, idx: usize) -> IrResult<ExprInstr> {
        if idx < self.instrs.len() {
            Ok(self.instrs.remove(idx))
        } else {
            Err(IrError::new(format!(
                "remove -- idx > len -- idx: {}, {:#?}",
                idx, self
            )))
        }
    }

    /// Inserts the given instruction at index `idx`, shifting the existing
    /// instructions to the "right".
    pub fn insert(&mut self, idx: usize, instrs: ExprInstr) -> IrResult<()> {
        if idx <= self.instrs.len() {
            self.instrs.insert(idx, instrs);
            Ok(())
        } else {
            Err(IrError::new(format!(
                "insert -- idx > len -- idx: {}, {:#?}",
                idx, self
            )))
        }
    }
}

impl Debug for BasicBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, " Label: {}", self.label)?;
        for instr in &self.instrs {
            writeln!(f, "  {:?}", instr)?;
        }
        if let Some(end_instr) = &self.end_instrs {
            writeln!(f, "  {:?}", end_instr)
        } else {
            writeln!(f, "  No end instruction")
        }
    }
}
