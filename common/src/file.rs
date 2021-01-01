use std::{
    fmt::{write, Debug},
    write,
};

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
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct FilePosition {
    pub file_nr: u64,
    pub offset: u64,
    pub length: u64,
    pub line_nr: u64,
    pub column_nr: u64,
}

impl FilePosition {
    pub fn new(file_nr: u64, offset: u64, length: u64, line_nr: u64, column_nr: u64) -> Self {
        Self {
            file_nr,
            offset,
            length,
            line_nr,
            column_nr,
        }
    }
}

impl Default for FilePosition {
    fn default() -> Self {
        Self {
            file_nr: 0,
            offset: 0,
            length: 0,
            line_nr: 0,
            column_nr: 0,
        }
    }
}

impl Debug for FilePosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.file_nr > 0 {
            f.debug_struct("FilePosition")
                .field("file_nr", &self.file_nr)
                .field("offset", &self.offset)
                .field("length", &self.length)
                .field("line_nr", &self.line_nr)
                .field("column_nr", &self.column_nr)
                .finish()
        } else {
            write!(f, "\"unspecified file position\"")
        }
    }
}
