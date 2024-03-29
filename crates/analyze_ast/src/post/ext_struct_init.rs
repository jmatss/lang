use common::{
    error::{LangError, LangErrorKind},
    token::expr::AdtInit,
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    ty::get::get_ident,
};

/// Makes sure that there are no initializations of externaly declared structs.
/// Since we have no idea about the members of the struct and its size during
/// compilation, it is not possible to create a instance of it. This analyzer
/// reports a error if that is the case.
pub struct ExtStructInit {
    errors: Vec<LangError>,
}

impl ExtStructInit {
    pub fn new() -> Self {
        Self {
            errors: Vec::default(),
        }
    }
}

impl Visitor for ExtStructInit {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_adt_init(&mut self, adt_init: &mut AdtInit, ctx: &mut TraverseCtx) {
        let ty_env_guard = ctx.ty_env.lock();

        let adt_type_id = adt_init.adt_type_id.unwrap();
        let adt_path = match get_ident(&ty_env_guard, adt_type_id) {
            Ok(adt_path_opt) => adt_path_opt.unwrap(),
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        let adt = match ctx.ast_ctx.get_adt(&ty_env_guard, &adt_path) {
            Ok(adt) => adt,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };
        let adt = adt.read();

        if !adt.has_definition {
            self.errors.push(LangError::new(
                format!(
                    "Tried to initialize struct \"{}\" which has no known definition.",
                    adt.name
                ),
                LangErrorKind::AnalyzeError,
                adt_init.file_pos,
            ));
        }
    }
}
