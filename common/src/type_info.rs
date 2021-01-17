use crate::file::FilePosition;

// TODO: Probably best to not have different variants in the enum. Should only
//       contain the `FilePosition` and there should be other ways to figure out
//       the kind of type, ex. by having access to the `Ty`.

/// Contains information about a type. This can contain ex. file position and
/// how it is used/what it belongs to.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeInfo {
    /// A "default"/"regular" type info that contains information that doesn't
    /// require any extra information other than the `FilePosition`.
    Default(FilePosition),
    DefaultOpt(Option<FilePosition>),

    Lit(Option<FilePosition>),
    Enum(FilePosition),
    VarUse(FilePosition),

    /// Since the type isn't written in the source code, this has no actual
    /// `FilePosition`. Use the position of the whole expr instead.
    BuiltInCall(FilePosition),
    FuncCall(FilePosition),

    /// The bool indicates if the `FilePosition` points to a hardcoded type or
    /// if it points to the variable name of the declaration. Since type annotations
    /// are optional, if no type is declared, the `FilePosition` points to the
    /// identifier of the variable decl instead.
    /// `true` indicates that the type is hardcoded, `false` otherwise.
    VarDecl(FilePosition, bool),

    /// Contains information about a type that is assigned to a enum member.
    ///
    /// The first (String, FilePosition) tuple contains information about the
    /// whole enum typedef (name + file_pos) and the second tuple is the name
    /// of the member and the position of the member inside the enum typedef.
    ///
    /// Currently, since enum members doesn't have explicit types set in the
    /// source code, the members file position will point to the identifier/name
    /// of the member.
    EnumMember((String, FilePosition), (String, FilePosition)),

    // TODO: Should this contain a reference to the structure that declared
    //       this generic?
    /// Contains information about a Generic type.
    Generic(FilePosition),

    /// A "default" `TypeInfo` that doesn't contain any information at all.
    None,
}

impl TypeInfo {
    pub fn file_pos(&self) -> Option<&FilePosition> {
        match self {
            TypeInfo::Default(file_pos)
            | TypeInfo::Enum(file_pos)
            | TypeInfo::VarUse(file_pos)
            | TypeInfo::BuiltInCall(file_pos)
            | TypeInfo::FuncCall(file_pos)
            | TypeInfo::VarDecl(file_pos, _)
            | TypeInfo::EnumMember((_, file_pos), _)
            | TypeInfo::Generic(file_pos) => Some(file_pos),

            TypeInfo::DefaultOpt(file_pos_opt) | TypeInfo::Lit(file_pos_opt) => {
                file_pos_opt.as_ref()
            }

            TypeInfo::None => None,
        }
    }
}
