use crate::{
    error::{IrError, IrResult},
    instruction::{EndInstr, Instr},
};

#[derive(Debug)]
pub struct BasicBlock {
    /// The name/label of this basic block.
    pub name: String,

    /// A list of all instructions inside the body of this basic block (excluding
    /// the last `end_instruction`).
    ///
    /// The order of the instructions inside the `instructions` vector is the
    /// "actual" order of the instructions and the flow of the program.
    instructions: Vec<Instr>,

    /// The last instruction of this basic block. This should some sort of
    /// exit/branch instruction.
    end_instruction: Option<EndInstr>,
}

impl BasicBlock {
    pub fn new(name: String) -> Self {
        Self {
            name,
            instructions: Vec::default(),
            end_instruction: None,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn set_name(&mut self, name: String) {
        self.name = name;
    }

    pub fn get_end_instruction(&mut self) -> Option<&EndInstr> {
        self.end_instruction.as_ref()
    }

    pub fn set_end_instruction(&mut self, end_instr: Option<EndInstr>) {
        self.end_instruction = end_instr;
    }

    pub fn has_end_instruction(&self) -> bool {
        self.end_instruction.is_some()
    }

    /// Adds a instruction to the end of this basic block (EXCLUDING end instruction).
    pub fn push(&mut self, instruction: Instr) {
        self.instructions.push(instruction);
    }

    /// Returns the instruction with index `idx`.
    pub fn get(&self, idx: usize) -> Option<&Instr> {
        self.instructions.get(idx)
    }

    /// Returns the first instruction of this basic block.
    pub fn first(&self) -> Option<&Instr> {
        self.instructions.first()
    }

    /// Returns the last instruction of this basic block (EXCLUDING end instruction).
    pub fn last(&self) -> Option<&Instr> {
        self.instructions.last()
    }

    /// Removes the instruction at index `idx`, shifting the remanining
    /// instructions to the "left".
    pub fn remove(&mut self, idx: usize) -> IrResult<Instr> {
        if idx < self.instructions.len() {
            Ok(self.instructions.remove(idx))
        } else {
            Err(IrError::new(format!(
                "remove -- idx > len -- idx: {}, {:#?}",
                idx, self
            )))
        }
    }

    /// Inserts the given instruction at index `idx`, shifting the existing
    /// instructions to the "right".
    pub fn insert(&mut self, idx: usize, instruction: Instr) -> IrResult<()> {
        if idx <= self.instructions.len() {
            self.instructions.insert(idx, instruction);
            Ok(())
        } else {
            Err(IrError::new(format!(
                "insert -- idx > len -- idx: {}, {:#?}",
                idx, self
            )))
        }
    }
}
