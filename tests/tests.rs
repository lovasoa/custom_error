#[macro_use]
extern crate custom_error;

use std::{fs::File, io, io::Read, num::ParseIntError, result::Result};

custom_error! {FileParseError
    Io{source: io::Error}         = "unable to read from the file",
    Format{source: ParseIntError} = "the file does not contain a valid integer",
    TooLarge{value:u8}            = "the number in the file ({value}) is too large"
}

fn parse_hex_file(filename: &str) -> Result<u8, FileParseError> {
    let mut contents = String::new();
    // The '?' notation can convert from generic errors to our custom error type
    File::open(filename)?.read_to_string(&mut contents)?;
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
    assert_eq!("unable to read from the file", parse_result.unwrap_err().to_string());
}