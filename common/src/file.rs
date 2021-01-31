use std::{fmt::Debug, write};

use crate::error::{LangError, LangErrorKind, LangResult};

pub type FileId = u64;

/// Contains information about a specific file. This file will be represented
/// with a "FileId" that is the `file_nr` inside the "FilePosition" struct.
#[derive(Debug, Clone, PartialEq)]
pub struct FileInfo {
    pub directory: Box<std::path::Path>,
    pub filename: String,
}

impl FileInfo {
    pub fn full_path(&self) -> String {
        self.directory
            .join(&self.filename)
            .to_str()
            .unwrap_or_else(|| panic!("Invalid file: {:#?}", self))
            .into()
    }
}

/// Contains information about where a token has been parsed from. This includes
/// the file, the byte offset in the file, the length (in bytes) of the token,
/// line nr and column nr.
#[derive(Copy, Clone, Default, PartialEq, Eq, Hash)]
pub struct FilePosition {
    pub file_nr: u64,
    pub offset: u64,
    pub length: u64,

    /// If the specific token only spans one line, `line_start` == `line_end`.
    pub line_start: u64,
    pub line_end: u64,

    /// `column_start` points to the position of the first byte of the token.
    /// `column_end` points to the last byte of the current token.
    /// `column_end` might be less than or equals to `column_start` if the token
    /// spans multiple lines.
    pub column_start: u64,
    pub column_end: u64,
}

impl FilePosition {
    pub fn new(file_nr: u64, offset: u64, line: u64, column: u64) -> Self {
        Self {
            file_nr,
            offset,
            length: 0,
            line_start: line,
            line_end: line,
            column_start: column,
            column_end: column,
        }
    }

    /// Sets the information about the end of the current `self` FilePosition
    /// from the given `file_pos_last`. This includes the end line/column number
    /// and the length of this `self` FilePosition.
    pub fn set_end(&mut self, file_pos_last: &FilePosition) -> LangResult<&mut Self> {
        self.set_end_pos(file_pos_last.line_end, file_pos_last.column_end)
            .set_end_offset(file_pos_last.offset + file_pos_last.length)
    }

    /// Sets the end positions for line and column.
    pub fn set_end_pos(&mut self, line_end: u64, column_end: u64) -> &mut Self {
        self.line_end = line_end;
        self.column_end = column_end;
        self
    }

    /// Sets the final offset/length. The given `end_offset` is compared to the
    /// already set `offset`(start offset), and the resulting diff is set as the
    /// `length` of this FilePosition.
    ///
    /// The `end_offset` should point to the index to the first byte AFTER the
    /// current token. This means that if the token is one byte long, `end_offset`
    /// will be 1 greater that `start_offset`.
    pub fn set_end_offset(&mut self, offset_end: u64) -> LangResult<&mut Self> {
        if offset_end >= self.offset {
            self.length = offset_end - self.offset;
            Ok(self)
        } else {
            Err(LangError::new(
                format!(
                    "start offset > offset_end ({} >= {})",
                    self.offset, offset_end
                ),
                LangErrorKind::LexError,
                Some(self.to_owned()),
            ))
        }
    }
}

impl Debug for FilePosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.file_nr > 0 {
            let mut dbg = f.debug_struct("FilePosition");
            dbg.field("file_nr", &self.file_nr)
                .field("offset", &self.offset)
                .field("length", &self.length);

            if self.line_start != self.line_end {
                dbg.field("line_start", &self.line_start)
                    .field("line_end", &self.line_end);
            } else {
                dbg.field("line", &self.line_start);
            }

            if self.line_start != self.line_end || self.column_start != self.column_end {
                dbg.field("column_start", &self.column_start)
                    .field("column_end", &self.column_end);
            } else {
                dbg.field("column", &self.column_start);
            }

            dbg.finish()
        } else {
            write!(f, "\"unspecified file position\"")
        }
    }
}
