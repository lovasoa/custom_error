/// Constructs a custom error type
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
#[macro_export]
macro_rules! custom_error {
    (
        $errtype:ident
        $(
            $field:ident
            $( { $( $attr_name:ident : $attr_type:ty ),* } )*
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
        };

        impl std::error::Error for $errtype {};


        impl std::fmt::Display for $errtype {
            fn fmt(&self, f: &mut std::fmt::Formatter)
                -> std::fmt::Result
            {
                match self {$(
                    $errtype::$field $( { $( $attr_name ),* } )* => {
                        write!(f, $msg $( $( , $attr_name = $attr_name )* )*)
                    }
                ),*}
            }
        }
    };
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
}
