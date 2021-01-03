use crate::generator::CodeGen;
use common::{
    error::CustomResult,
    token::{
        ast::AstToken,
        block::{BlockHeader, Enum, Struct},
    },
};
use inkwell::module::Linkage;
use log::warn;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
};

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    /// Compile all declarations of types: structs, enums and interfaces.
    /// This will be done at the start of the code generation so that one
    /// doesn't have do declare prototypes manual in the source before the use
    /// of the type.
    /// This function shall be ran before the function/method prototypes
    /// are compiled since they might contains references to types.
    pub(super) fn compile_type_decl(&mut self, ast_token: &AstToken) -> CustomResult<()> {
        self.cur_file_pos = ast_token.file_pos().cloned().unwrap_or_default();

        // The key is the name of the structure and the value is the actual
        // structure that is to be compiled.
        let mut structs: HashMap<String, Rc<RefCell<Struct>>> = HashMap::default();
        let mut enums: HashMap<String, Rc<RefCell<Enum>>> = HashMap::default();

        // The key is the name of a structure and the value set contains names of
        // structures that is referenced from the given "key" structure. This does
        // NOT include recursive dependencies, only direct "top level" references.
        let mut references: HashMap<String, HashSet<String>> = HashMap::default();

        // Step 1: Find all structures and references between them.
        // Step 2: Figure out the correct order to compile them in.
        self.step1(ast_token, &mut structs, &mut enums, &mut references)?;

        warn!(
            "structs: {:#?}\nenums: {:#?}\nreferences: {:#?}\n",
            structs, enums, references
        );

        for ident in self.step2(&mut references)? {
            warn!("compile ident: {}", ident);
            if let Some(struct_) = structs.get(&ident) {
                self.compile_struct(&struct_.borrow())?;
            } else if let Some(enum_) = enums.get(&ident) {
                self.compile_enum(&enum_.borrow())?;
            } else {
                return Err(self.err(
                    format!(
                        "Unable to find structure with name \"{}\" when compiling type decl.",
                        &ident
                    ),
                    None,
                ));
            }
        }

        Ok(())
    }

    /// Iterate through all types in the structure and see if it uses/references
    /// other structures. In those cases, the referenced structures would need to
    /// be compiled before the referencing structures.
    ///
    /// This function will go through all structures in the given `ast_token`
    /// root and populated the ``structs`/`enums` maps with references to the
    /// structures. The `references` map will be populated with the references
    /// between the structures.
    fn step1(
        &mut self,
        ast_token: &AstToken,
        structs: &mut HashMap<String, Rc<RefCell<Struct>>>,
        enums: &mut HashMap<String, Rc<RefCell<Enum>>>,
        references: &mut HashMap<String, HashSet<String>>,
    ) -> CustomResult<()> {
        if let AstToken::Block(header, _, id, body) = ast_token {
            self.cur_block_id = *id;

            match header {
                BlockHeader::Struct(struct_) => {
                    let struct_name = struct_.borrow().name.clone();
                    structs.insert(struct_name.clone(), Rc::clone(struct_));
                    let mut local_references = HashSet::default();

                    if let Some(members) = &struct_.borrow().members {
                        for member in members {
                            if let Some(ty) = &member.borrow().ty {
                                if let Some(names) = ty.get_structure_names() {
                                    for name in names {
                                        if name != struct_name {
                                            local_references.insert(name);
                                        }
                                    }
                                }
                            }
                        }
                    }

                    references.insert(struct_name, local_references);
                }
                BlockHeader::Enum(enum_) => {
                    let enum_name = enum_.borrow().name.clone();
                    enums.insert(enum_name.clone(), Rc::clone(enum_));
                    let mut local_references = HashSet::default();

                    if let Some(members) = &enum_.borrow().members {
                        for member in members {
                            if let Some(ty) = &member.borrow().ty {
                                if let Some(names) = ty.get_structure_names() {
                                    for name in names {
                                        if name != enum_name {
                                            local_references.insert(name);
                                        }
                                    }
                                }
                            }
                        }
                    }

                    references.insert(enum_name, local_references);
                }
                BlockHeader::Interface(interface) => {
                    panic!("TODO: interface");
                }
                _ => (),
            }

            for token in body {
                self.step1(token, structs, enums, references)?;
            }
        }

        Ok(())
    }

    /// Given the references between structures in `references`, figures out
    /// the order in which the structures should be compiled so that there are
    /// no problems with the depenencies between them. Returns a error if a cyclic
    /// dependency is found.
    fn step2(
        &mut self,
        references: &mut HashMap<String, HashSet<String>>,
    ) -> CustomResult<Vec<String>> {
        let mut order: Vec<String> = Vec::with_capacity(references.len());

        for cur_ident in references.keys() {
            let mut idx = 0;

            for prev_ident in &order {
                let prev_references_cur = CodeGen::contains(cur_ident, prev_ident, &references);
                let cur_references_prev = CodeGen::contains(prev_ident, cur_ident, &references);

                if prev_references_cur && cur_references_prev {
                    return Err(self.err(
                        format!(
                            "Cyclic dependency between structures \"{}\" and \"{}\".",
                            cur_ident, prev_ident
                        ),
                        None,
                    ));
                } else if prev_references_cur {
                    // Can't insert the "current" structure before the "previous"
                    // since it is being referenced from it. Insert into `order`
                    // at this index so that "current" gets compiled before "previous".
                    break;
                } else {
                    // else if cur_references_prev = >
                    //   "Current" references "previous", so "previous" needs to
                    //   be compiled first. Keep iterating and find a spot after
                    //   "previous" to insert it.
                    // else
                    //   The two structures has no references between each other.
                    //   Keep iterating to find a spot to insert it as "late"
                    //   as possible.
                }

                idx += 1;
            }

            if idx > order.len() {
                order.push(cur_ident.into());
            } else {
                order.insert(idx, cur_ident.into());
            }
        }

        warn!("order: {:#?}", order);

        Ok(order)
    }

    // TODO: This is not effective. Will this be a problem for programs with
    //       a lot of structures?
    /// Checks if the structure with name `cur_ident` are referenced from the
    /// structure with name `ref_ident` recursively. This will find cyclic
    /// dependencies.
    fn contains(
        cur_ident: &str,
        ref_ident: &str,
        references: &HashMap<String, HashSet<String>>,
    ) -> bool {
        // HashSet used to detect cyclic dependencies.
        let mut seen_idents = HashSet::default();
        CodeGen::contains_rec(cur_ident, ref_ident, references, &mut seen_idents)
    }

    fn contains_rec(
        cur_ident: &str,
        ref_ident: &str,
        references: &HashMap<String, HashSet<String>>,
        seen_idents: &mut HashSet<String>,
    ) -> bool {
        let ref_references = if let Some(ref_references) = references.get(ref_ident) {
            ref_references
        } else {
            return false;
        };

        if ref_references.contains(cur_ident) {
            return true;
        } else {
            for nested_ident in ref_references {
                let is_cyclic_dependency = seen_idents.contains(nested_ident);
                seen_idents.insert(nested_ident.into());

                if is_cyclic_dependency
                    || CodeGen::contains_rec(cur_ident, nested_ident, references, seen_idents)
                {
                    return true;
                }
            }
        }

        false
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
                BlockHeader::Implement(_) => {
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
