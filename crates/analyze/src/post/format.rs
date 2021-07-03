use common::{
    error::{LangError, LangErrorKind, LangResult},
    file::FilePosition,
    iter::TokenIter,
    token::{
        expr::{Argument, BuiltInCall, Expr, FormatPart},
        lit::Lit,
    },
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    ty::{get::get_ident, is::is_primitive, ty_env::TyEnv},
};

/// Parses and "analyzes" the format string inside `@format(..)` calls.
/// This information will be inserted into the `BuiltInCall` and will be used
/// during code generation to simplify the creation of the formatted string.
///
/// Also checks valid types for the variadic arguments. For now the only allowed
/// types are `std::types::StringView` and any primitive (that can be turned
/// into `StringView`).
pub struct FormatParser {
    errors: Vec<LangError>,
}

impl FormatParser {
    pub fn new() -> Self {
        Self {
            errors: Vec::default(),
        }
    }

    fn parse_format(
        &self,
        str_lit: &str,
        args: &[Argument],
        file_pos: Option<FilePosition>,
    ) -> LangResult<Vec<FormatPart>> {
        // TODO: Make possible to use without `str_lit` needing to be mut.
        let mut iter = TokenIter::new(unsafe {
            ((str_lit as *const str) as *mut str)
                .as_mut()
                .unwrap()
                .as_bytes_mut()
        });

        let mut format_parts = Vec::default();

        // The count of all arguments excluding the first string literal argument.
        let varargs_count = args.len() - 1;

        let mut is_in_format_indicator = false;
        let mut cur_string_part = Vec::default();
        let mut cur_arg_idx = 1;

        while let Some(byte) = iter.next() {
            match byte {
                b'{' => {
                    if !is_in_format_indicator {
                        if !cur_string_part.is_empty() {
                            let string_part =
                                std::str::from_utf8(&cur_string_part).map_err(|_| {
                                    LangError::new(
                                        format!(
                                    "Unable to convert format text to string at pos {}: {:#?}",
                                    iter.pos(),
                                    std::str::from_utf8(iter.get_items())
                                ),
                                        LangErrorKind::AnalyzeError,
                                        file_pos,
                                    )
                                })?;
                            format_parts.push(FormatPart::String(string_part.into()));
                            cur_string_part.clear();
                        }

                        is_in_format_indicator = true;
                    } else {
                        return Err(LangError::new(
                            format!(
                                "Found start of nested format indicator in format literal at pos \"{}\": {:?}",
                                iter.pos(),
                                std::str::from_utf8(iter.get_items())
                            ),
                            LangErrorKind::AnalyzeError,
                            file_pos,
                        ));
                    }
                }

                b'}' => {
                    if is_in_format_indicator {
                        if cur_arg_idx <= varargs_count {
                            format_parts.push(FormatPart::Arg(
                                args.get(cur_arg_idx).unwrap().value.clone(),
                            ));

                            cur_arg_idx += 1;
                            is_in_format_indicator = false;
                        } else {
                            return Err(LangError::new(
                                format!(
                                    "Built-in format call has more args than format indicators. \
                                    Format string: \"{}\". Amount of arguments: {}",
                                    str_lit, varargs_count
                                ),
                                LangErrorKind::AnalyzeError,
                                file_pos,
                            ));
                        }
                    } else {
                        return Err(LangError::new(
                            format!(
                                "Found end of format indicator before a start symbol at pos \"{}\": {:?}",
                                iter.pos(),
                                str_lit
                            ),
                            LangErrorKind::AnalyzeError,
                            file_pos,
                        ));
                    }
                }

                _ => {
                    cur_string_part.push(byte);
                }
            }
        }

        if is_in_format_indicator {
            return Err(LangError::new(
                format!(
                    "Found start of format indicator without an end in: {}",
                    str_lit
                ),
                LangErrorKind::AnalyzeError,
                file_pos,
            ));
        }

        // Need to "flush" any text we have accumulated in the loop above before
        // we exit this function.
        if !cur_string_part.is_empty() {
            let string_part = std::str::from_utf8(&cur_string_part).map_err(|_| {
                LangError::new(
                    format!(
                        "Unable to convert format text to string at pos {}: {:#?}",
                        iter.pos(),
                        std::str::from_utf8(iter.get_items())
                    ),
                    LangErrorKind::AnalyzeError,
                    file_pos,
                )
            })?;
            format_parts.push(FormatPart::String(string_part.into()));
        }

        Ok(format_parts)
    }

    fn verify_arg_types(&self, ty_env: &TyEnv, built_in_call: &BuiltInCall) -> LangResult<()> {
        let mut idx = 0;

        for arg in &built_in_call.arguments {
            if idx == 0 {
                idx += 1;
                continue;
            }

            let type_id = arg.value.get_expr_type()?;
            if !get_ident(&ty_env, type_id)?
                .map(|path| path.last().map(|part| part.0 == "StringView"))
                .flatten()
                .unwrap_or(false)
                && !is_primitive(&ty_env, type_id)?
            {
                // Error because the argument type is neither StringView or primitive.
                LangError::new(
                    format!(
                        "Found invalid type in `@format()` argument at idx {}, pos: {:?}",
                        idx,
                        arg.value.file_pos()
                    ),
                    LangErrorKind::AnalyzeError,
                    Some(built_in_call.file_pos),
                );
            }

            idx += 1;
        }

        Ok(())
    }
}

impl Visitor for FormatParser {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_built_in_call(&mut self, built_in_call: &mut BuiltInCall, ctx: &mut TraverseCtx) {
        if built_in_call.name == "format" {
            let arg_len = built_in_call.arguments.len();
            if arg_len < 1 {
                let err = LangError::new(
                    "`@format()` call had no arguments, expected atleast one.".into(),
                    LangErrorKind::AnalyzeError,
                    Some(built_in_call.file_pos),
                );
                self.errors.push(err);
                return;
            }

            let format_arg = built_in_call.arguments.first().unwrap();
            let (str_lit, file_pos) =
                if let Expr::Lit(Lit::String(str_lit), _, file_pos) = &format_arg.value {
                    (str_lit, file_pos)
                } else {
                    let err = LangError::new(
                        "Built-in `@format()` first argument not a string literal.".into(),
                        LangErrorKind::AnalyzeError,
                        Some(built_in_call.file_pos),
                    );
                    self.errors.push(err);
                    return;
                };

            match self.parse_format(str_lit, &built_in_call.arguments, file_pos.to_owned()) {
                Ok(format_parts) => built_in_call.format_parts = Some(format_parts),
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            }

            if let Err(err) = self.verify_arg_types(&ctx.ty_env.lock().unwrap(), &built_in_call) {
                self.errors.push(err);
            }
        }
    }
}
