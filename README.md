# Rust custom error

This is a rust crate containing a macro that should make it more
easy to define custom errors without having to write a lot of boilerplate code.

The `custom_error!` macro included in this crate takes a type name
and a list of error cases and generates a rust enumeration of all the test cases,
together with two the required `impl` blocks implementing `std::error::Error`
and `std::fmt::Display`. 

You can now write

```rust
custom_error!(MyError
    Unknown{code:u8} = "unknown error with code {code}.",
    Err41            = "Sit by a lake"
);
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

# Examples

### Simple error
```rust
extern crate custom_error;
use custom_error::custom_error;

custom_error!(MyError
    Bad      = "Something bad happened",
    Terrible = "This is a very serious error!!!"
);

assert_eq!("Something bad happened",          MyError::Bad.to_string());
assert_eq!("This is a very serious error!!!", MyError::Terrible.to_string());
```

### Custom error with parameters
```rust
extern crate custom_error;
use custom_error::custom_error;

custom_error!(SantaError
    BadChild{name:String, foolishness:u8} = "{name} has been bad {foolishness} times this year",
    TooFar                                = "The location you indicated is too far from the north pole",
    InvalidReindeer{legs:u8}              = "The reindeer has {legs} legs"
);

assert_eq!(
    "Thomas has been bad 108 times this year",
    SantaError::BadChild{
        name: "Thomas".into(),
        foolishness: 108
    }.to_string());
assert_eq!(
    "The location you indicated is too far from the north pole",
    SantaError::TooFar.to_string()
);
assert_eq!(
    "The reindeer has 8 legs",
    SantaError::InvalidReindeer{legs:8}.to_string()
);
```