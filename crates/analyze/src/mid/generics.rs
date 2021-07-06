use common::{
    error::LangError,
    path::LangPathPart,
    token::{
        ast::AstToken,
        block::{Block, BlockHeader},
    },
    traverse::{traverse_ctx::TraverseCtx, traverser::traverse, visitor::Visitor},
    ty::{generics::Generics, replace::replace_gens, to_string::to_string_path, type_id::TypeId},
};

/// Iterates through "generic" parameters tied to ADTs and functions, and replaces
/// the uses of the generics with "Generic" types instead of the parsed
/// "UnknownIdent". This is done so that generics can be handled differently
/// during the type inference stage.
pub struct GenericsAnalyzer {
    errors: Vec<LangError>,
}

impl GenericsAnalyzer {
    pub fn new() -> Self {
        Self {
            errors: Vec::default(),
        }
    }
}

impl Visitor for GenericsAnalyzer {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    /// "Rewrites" the types of the generic member types to "Generic"s for
    /// the structure members.
    fn visit_struct(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Struct(struct_),
            ..
        } = &block
        {
            let struct_ = struct_.as_ref().read().unwrap();
            if let (Some(generics), members) = (&struct_.generics, &struct_.members) {
                for member in members {
                    if let Some(type_id) = member.as_ref().write().unwrap().ty.as_mut() {
                        if let Err(err) =
                            replace_gens(&mut ctx.ty_env.lock().unwrap(), *type_id, generics)
                        {
                            self.errors.push(err);
                        }
                    }
                }
            }
        }
    }

    fn visit_union(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Union(union),
            ..
        } = &block
        {
            let union = union.as_ref().read().unwrap();
            if let (Some(generics), members) = (&union.generics, &union.members) {
                for member in members {
                    if let Some(type_id) = member.as_ref().write().unwrap().ty.as_mut() {
                        if let Err(err) =
                            replace_gens(&mut ctx.ty_env.lock().unwrap(), *type_id, generics)
                        {
                            self.errors.push(err);
                        }
                    }
                }
            }
        }
    }

    /// "Rewrites" generics parsed as "UnknownIdent"s to "Generic"s by matching
    /// the identifiers with known names for the generics defined on the structure.
    /// This will be done for both the method headers and everything in their bodies.
    fn visit_impl(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Implement(impl_path, ..),
            body,
            ..
        } = block
        {
            let full_impl_path = match ctx.ast_ctx.get_module(ctx.block_id) {
                Ok(Some(mut full_impl_path)) => {
                    let impl_ident = impl_path.last().unwrap().0.clone();
                    full_impl_path.push(LangPathPart(impl_ident, None));
                    full_impl_path
                }
                Ok(None) => {
                    let mut full_impl_path = impl_path.clone();
                    let last_part = full_impl_path.pop().unwrap();
                    full_impl_path.push(LangPathPart(last_part.0, None));
                    full_impl_path
                }
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            // TODO: Implement generics for iterfaces and enums (?).

            let adt_generics = if let Ok(adt) = ctx
                .ast_ctx
                .get_adt(&ctx.ty_env.lock().unwrap(), &full_impl_path)
            {
                adt.as_ref()
                    .read()
                    .unwrap()
                    .generics
                    .clone()
                    .unwrap_or_default()
            } else if ctx
                .ast_ctx
                .get_trait(&ctx.ty_env.lock().unwrap(), &full_impl_path)
                .is_ok()
            {
                Generics::empty()
            } else {
                let err = ctx.ast_ctx.err(format!(
                    "Unable to find ADT/Trait for impl block with name \"{}\".",
                    to_string_path(&ctx.ty_env.lock().unwrap(), &full_impl_path),
                ));
                self.errors.push(err);
                return;
            };

            // Iterate through the body of one method at a time and replace all
            // "UnknownIdent"s representing generics to "Generic"s.
            for method in body {
                let func_generics = if let AstToken::Block(Block {
                    header: BlockHeader::Fn(func),
                    ..
                }) = method
                {
                    func.as_ref().read().unwrap().generics.clone()
                } else {
                    panic!("Not method in impl block: {:#?}", method);
                };

                // No generics declared on either ADT/Trait or function, early skip.
                if adt_generics.is_empty() && func_generics.is_none() {
                    continue;
                }

                // Replaces any generics declared on the function.
                if let Some(func_generics) = func_generics {
                    let mut func_replacer = FuncGenericsReplacer::new(&func_generics);
                    if let Err(mut errs) = traverse(ctx, &mut func_replacer, method) {
                        self.errors.append(&mut errs);
                        return;
                    }
                }

                // Replaces any generics declared on the ADT/Trait.
                if !adt_generics.is_empty() {
                    let mut adt_replacer = FuncGenericsReplacer::new(&adt_generics);
                    if let Err(mut errs) = traverse(ctx, &mut adt_replacer, method) {
                        self.errors.append(&mut errs);
                        return;
                    }
                }
            }
        }
    }
}

struct FuncGenericsReplacer<'a> {
    adt_generics: &'a Generics,
    errors: Vec<LangError>,
}

/// Used when replacing generics in methods containing to a specific generic
/// implementation. This will be used to replace all types in the body of the
/// methods.
impl<'a> FuncGenericsReplacer<'a> {
    pub fn new(adt_generics: &'a Generics) -> Self {
        Self {
            adt_generics,
            errors: Vec::default(),
        }
    }
}

impl<'a> Visitor for FuncGenericsReplacer<'a> {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_type(&mut self, type_id: &mut TypeId, ctx: &mut TraverseCtx) {
        if let Err(err) = replace_gens(&mut ctx.ty_env.lock().unwrap(), *type_id, self.adt_generics)
        {
            self.errors.push(err);
        }
    }
}
