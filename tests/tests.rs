#[macro_use]
extern crate custom_error;

use std::{fs::File, io, io::ErrorKind, io::Read, num::ParseIntError, result::Result};

custom_error! {
    OpenFileError
        NotFound{filename: String} = "Tried to open '{filename}', but it doesn't exist",
        Other                      = "An unknown I/O error occured.",
}

/// Opens a file with a verbose error on failure
fn open_file_verbose(filename: &str) -> Result<File, OpenFileError> {
    File::open(filename).map_err(|e| match e.kind() {
        ErrorKind::NotFound => OpenFileError::NotFound {
            filename: filename.to_string(),
        },
        _ => OpenFileError::Other,
    })
}

custom_error! {FileParseError
    Open{source: OpenFileError}   = @{ source }, // Chained custom error
    Io{source: io::Error}         = "I/O error",
    Format{source: ParseIntError} = "the file does not contain a valid integer",
    TooLarge{value:u8}            = "the number in the file ({value}) is too large"
}

fn parse_hex_file(filename: &str) -> Result<u8, FileParseError> {
    let mut contents = String::new();
    // The '?' notation can convert from generic errors to our custom error type
    open_file_verbose(filename)?.read_to_string(&mut contents)?;
    let value = u8::from_str_radix(&contents, 16)?;
    if value > 42 {
        Err(FileParseError::TooLarge { value })
    } else {
        Ok(value)
    }
}

#[test]
fn main() {
    let parse_result = parse_hex_file("/i'm not a file/");
    assert_eq!(
        "Tried to open '/i'm not a file/', but it doesn't exist",
        parse_result.unwrap_err().to_string()
    );
}
