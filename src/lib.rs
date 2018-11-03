/// Constructs a custom error type.
///
/// # Examples
///
/// ### Simple error
///
/// ```
/// use custom_error::custom_error;
///
/// custom_error!(MyError
///     Bad      = "Something bad happened",
///     Terrible = "This is a very serious error!!!"
/// );
/// assert_eq!("Something bad happened",          MyError::Bad.to_string());
/// assert_eq!("This is a very serious error!!!", MyError::Terrible.to_string());
/// ```
///
/// ### Custom error with parameters
/// ```
/// use custom_error::custom_error;
///
/// custom_error!(SantaError
///     BadChild{name:String, foolishness:u8} = "{name} has been bad {foolishness} times this year",
///     TooFar                                = "The location you indicated is too far from the north pole",
///     InvalidReindeer{legs:u8}              = "The reindeer has {legs} legs"
/// );
///
/// assert_eq!(
///     "Thomas has been bad 108 times this year",
///     SantaError::BadChild{
///         name: "Thomas".into(),
///         foolishness: 108
///     }.to_string());
///
/// assert_eq!(
///     "The location you indicated is too far from the north pole",
///     SantaError::TooFar.to_string()
/// );
///
/// assert_eq!(
///     "The reindeer has 8 legs",
///     SantaError::InvalidReindeer{legs:8}.to_string()
/// );
/// ```
///
///  ### Automatic conversion from other error types
///
/// You can add a special field named `source` to your error types.
///
/// Use this field to include the lower-level source of the error.
/// It will be used in the error
/// [`source()`](https://doc.rust-lang.org/std/error/trait.Error.html#method.source)
/// method, and automatic conversion from the source error type to your custom error type will be possible
/// (your error type will implement `From<SourceErrorType>`).
///
/// #### limitations
///  * You cannot have several error cases that contain a single *source* field of the same type:
///         `custom_error!(E A{source:X} B{source:Y})` is allowed, but
///         `custom_error!(E A{source:X} B{source:X})` is forbidden.
///  * If the source field is not the only one, then the automatic conversion
///    will not be implemented.
///
/// ```
/// use custom_error::custom_error;
/// use std::{io, io::Read, fs::File, result::Result};
///
/// custom_error!(MyError
///     IO{source: io::Error} = "input/output error",
///     Unknown               = "unknown error"
/// );
///
/// fn read_file(filename: &str) -> Result<String, MyError> {
///     let mut res = String::new();
///     File::open(filename)?.read_to_string(&mut res)?;
///     Ok(res)
/// }
///
/// assert_eq!(
///     "input/output error",
///     read_file("/i'm not a file/").unwrap_err().to_string()
/// )
/// ```
///
#[macro_export]
macro_rules! custom_error {
    (
        $errtype:ident
        $(
            $field:ident
            $( {
                    $( $attr_name:ident : $attr_type:ty ),*
            } )*
            =
            $msg:expr
         ),*
    ) => {
        #[derive(Debug)]
        enum $errtype {
            $(
                $field
                $( { $( $attr_name : $attr_type ),* } )*
            ),*
        }

        impl std::error::Error for $errtype {
            fn source(&self) -> Option<&(dyn std::error::Error + 'static)>
            {
                #[allow(unused_variables, unreachable_code)]
                match self {$(
                    $errtype::$field $( { $( $attr_name ),* } )* => {
                        $( $( $crate::return_if_source!($attr_name, $attr_name) );* )*;
                        None
                    }
                ),*}
            }
        }

        $(
            $( $crate::impl_error_conversion!{$($attr_name, $attr_name, $attr_type,)* $errtype, $field} )*
        )*

        impl std::fmt::Display for $errtype {
            fn fmt(&self, formatter: &mut std::fmt::Formatter)
                -> std::fmt::Result
            {
                match self {$(
                    $errtype::$field $( { $( $attr_name ),* } )* => {
                        write!(
                            formatter,
                            concat!($msg $( $( , "{", stringify!($attr_name), ":.0}" )* )*)
                            $( $( , $attr_name = $attr_name.to_string() )* )*
                        )
                    }
                ),*}
            }
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! return_if_source {
    (source, $attr_name:ident) => { {return Some($attr_name)} };
    ($($_:tt)*) => {};
}

#[doc(hidden)]
#[macro_export]
macro_rules! impl_error_conversion {
    // implement From<Source> only when there is a single attribute and it is named 'source'
    (source, $source:ident, $error_type:ty, $errtype:ident, $field:ident) => {
        impl From<$error_type> for $errtype {
            fn from(source: $error_type) -> Self {
                $errtype::$field { source }
            }
        }
    };
    ($($_:tt)*) => {};
}

#[cfg(test)]
mod tests {
    #[test]
    fn single_error_case() {
        custom_error!(MyError Bad="bad");
        assert_eq!("bad", MyError::Bad.to_string())
    }

    #[test]
    #[allow(dead_code)]
    fn three_error_cases() {
        custom_error!(MyError NotPerfect=":/", Bad="bad", Awfull="arghhh");
        assert_eq!("arghhh", MyError::Awfull.to_string())
    }

    #[test]
    fn with_error_data() {
        custom_error!(MyError
            Bad                            = "bad",
            Catastrophic{broken_things:u8} = "{broken_things} things are broken"
        );
        assert_eq!("bad", MyError::Bad.to_string());
        assert_eq!(
            "9 things are broken",
            MyError::Catastrophic { broken_things: 9 }.to_string()
        );
    }

    #[test]
    fn with_multiple_error_data() {
        custom_error!(E X{a:u8, b:u8, c:u8} = "{c} {b} {a}");

        assert_eq!("3 2 1", E::X { a: 1, b: 2, c: 3 }.to_string());
    }

    #[test]
    fn source() {
        use std::{io, error::Error};
        custom_error!(E A{source: io::Error}="");
        let source: io::Error = io::ErrorKind::InvalidData.into();
        assert_eq!(
            source.to_string(),
            E::A { source }.source().unwrap().to_string()
        );
    }

    #[test]
    fn from_source() {
        use std::io;
        custom_error!(E A{source: io::Error}="bella vita");
        let source = io::Error::from(io::ErrorKind::InvalidData);
        assert_eq!("bella vita", E::from(source).to_string());
    }

    #[test]
    #[allow(dead_code)]
    fn with_source_and_others() {
        use std::{io, error::Error};
        custom_error!(MyError Zero="", One{x:u8}="", Two{x:u8, source:io::Error}="{x}");
        fn source() -> io::Error { io::ErrorKind::AlreadyExists.into() };
        let my_err = MyError::Two { x: 42, source: source() };
        assert_eq!("42", my_err.to_string());
        assert_eq!(source().to_string(), my_err.source().unwrap().to_string());
    }
}
