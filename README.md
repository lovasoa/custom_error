# Rust custom error

This crate contains a macro that should make it easier
to define custom errors without having to write a lot of boilerplate code.

The `custom_error!` macro included in this crate takes a type name
and a list of possible errors and generates a rust enumeration for all the cases,
together with the required `impl` blocks implementing `std::error::Error`
and `std::fmt::Display`. 

If you only have a single case for an error you can also generate a struct
instead of an enum.

You can now write:

```rust
extern crate custom_error;
use custom_error::custom_error;

// Note the use of braces rather than parentheses.
custom_error!{MyError
    Unknown{code:u8} = "unknown error with code {code}.",
    Err41            = "Sit by a lake"
}
```

instead of

```rust
#[derive(Debug)]
enum MyError {
    Unknown { code: u8 },
    Err41,
}

impl std::error::Error for MyError {}

impl std::fmt::Display for MyError {
    fn fmt(&self, f: &mut std::fmt::Formatter)
    -> std::fmt::Result {
        match self {
            MyError::Unknown { code } => write!(f, "unknown error with code {}." , code),
            MyError::Err41 => write!(f, "Sit by a lake")
        }
    }
}
```

If you only have a single error case you can also generate a struct:
```rust
extern crate custom_error;
use custom_error::custom_error;

// Note the use of braces rather than parentheses.
custom_error!{MyError{code:u8} = "error with code {code}."}
```

instead of

```rust
#[derive(Debug)]
struct MyError {
    code: u8,
}

impl std::error::Error for MyError {}

impl std::fmt::Display for MyError {
    fn fmt(&self, f: &mut std::fmt::Formatter)
    -> std::fmt::Result {
        write!(f, "error with code {}." , self.code)
    }
}
```

## Simple error

To define a simple error, you only have to indicate three things:
 * the name of your custom error type,
 * the name of the different error cases,
 * a human-readable description for each case.

```rust
extern crate custom_error;
use custom_error::custom_error;

custom_error!{MyError
    Bad      = "Something bad happened",
    Terrible = "This is a very serious error!!!"
}
```

## Custom error with parameters

You can store data inside your errors.
In order to do so, indicate the name and types of the fields to want to store in curly braces
after an error type.

```rust
extern crate custom_error;
use custom_error::custom_error;

custom_error!{SantaError
    BadChild{name:String, foolishness:u8} = "{name} has been bad {foolishness} times this year",
    TooFar                                = "The location you indicated is too far from the north pole",
    InvalidReindeer{legs:u8}              = "The reindeer has {legs} legs"
}

assert_eq!(
    "Thomas has been bad 108 times this year",
    SantaError::BadChild{name: "Thomas".into(), foolishness: 108}.to_string());
```

The error messages can reference your parameters using braces (`{parameter_name}`).
If you need some custom logic to display your parameters, you can use
[advanced custom error messages](#advanced-custom-error-messages).

## Wrapping other error types

If the cause of your error is another lower-level error, you can indicate that
by adding a special `source` field to one of your error cases.

Thus, you can make your custom error wrap other error types,
and the conversion from these foreign error types will be implemented automatically.

```rust
#[macro_use] extern crate custom_error;
use std::{io, io::Read, fs::File, result::Result, num::ParseIntError};

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

fn main() {
    let parse_result = parse_hex_file("/i'm not a file/");
    assert_eq!("unable to read from the file", parse_result.unwrap_err().to_string());
}
```

## Visibility

You can make an error type public by adding the `pub` keyword
at the beginning of the declaration.

```rust
custom_error!{pub MyError A="error a" B="error b"}
```

## Advanced custom error messages

If you want to use error messages that you cannot express with
simple formatting strings, you can generate your error messages with
custom code.

In the following example, we use this feature to display a
different error message based on the cause of the underlying IO error.

```rust
custom_error!{ pub MyError
    Io{source: Error} = @{
        match source.kind() {
            NotFound => "The file does not exist",
            TimedOut => "The operation timed out",
            _ => "unknown error",
        }
    },
    Unknown = "unknown error"
}
```
