/// Constructs a custom error type.
///
/// # Examples
///
/// ### Simple error
///
/// ```
/// use custom_error::custom_error;
///
/// custom_error!{ pub MyError
///     Bad      = "Something bad happened",
///     Terrible = "This is a very serious error!!!"
/// }
/// assert_eq!("Something bad happened",          MyError::Bad.to_string());
/// assert_eq!("This is a very serious error!!!", MyError::Terrible.to_string());
/// ```
///
/// ### Custom error with parameters
/// ```
/// use custom_error::custom_error;
///
/// custom_error!{SantaError
///     BadChild{name:String, foolishness:u8} = "{name} has been bad {foolishness} times this year",
///     TooFar                                = "The location you indicated is too far from the north pole",
///     InvalidReindeer{legs:u8}              = "The reindeer has {legs} legs"
/// }
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
/// custom_error!{MyError
///     IO{source: io::Error} = "input/output error",
///     Unknown               = "unknown error"
/// }
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
///  ### Custom formatting function for error messages
///
/// If the format string syntax is not enough to express your complex error formatting needs,
/// you can use custom code to generate your error description.
///
/// ```
/// use custom_error::custom_error;
///
/// static lang : &'static str = "FR";
///
/// # fn localize(_:&str, _:&str) -> &'static str { "Un problème est survenu" }
///
/// custom_error!{ pub MyError
///     Problem      = @{ localize(lang, "A problem occurred") },
/// }
///
/// assert_eq!("Un problème est survenu", MyError::Problem.to_string());
/// ```
///
/// ```
/// use custom_error::custom_error;
/// use std::io::Error;
/// use std::io::ErrorKind::*;
///
/// custom_error!{ pub MyError
///     Io{source: Error} = @{
///         match source.kind() {
///             NotFound => "The file does not exist",
///             TimedOut => "The operation timed out",
///             _ => "unknown error",
///         }
///     },
/// }
///
/// assert_eq!("The operation timed out", MyError::Io{source: TimedOut.into()}.to_string());
/// ```

#[macro_export]
macro_rules! custom_error {
    (pub $($tt:tt)*) => { $crate::custom_error!{ (pub) $($tt)* } };

    (
        $( ($prefix:tt) )* // `pub` marker
        $errtype:ident // Name of the error type to generate
        $( < $(
            $type_param:tt // Optional type parameters for generic error types
            ),*
        > )*
        $(
            $field:ident // Name of an error variant
            $( { $(
                $attr_name:ident // Name of an attribute of the error variant
                :
                $($attr_type:ident)::* // type of the attribute
                $(< $($attr_type_param:tt),* >)* // Generic (lifetime & type) parameters for the attribute's type
            ),* } )*
            =
            $( @{ $($msg_fun:tt)* } )*
            $($msg:expr)* // The human-readable error message
         ),*
         $(,)* // Trailing comma
    ) => {
        #[derive(Debug)]
        $($prefix)* enum $errtype $( < $($type_param),* > )* {
            $(
                $field
                $( { $( $attr_name : $($attr_type)::* $(< $($attr_type_param),* >)* ),* } )*
            ),*
        }

        $crate::add_type_bounds! {
        ( $($($type_param),*)* )
        (std::fmt::Debug + std::fmt::Display)
        { impl <} {> std::error::Error
            for $errtype $( < $($type_param),* > )*
        {
            fn source(&self) -> Option<&(dyn std::error::Error + 'static)>
            {
                #[allow(unused_variables, unreachable_code)]
                match self {$(
                    $errtype::$field $( { $( $attr_name ),* } )* => {
                        $( $(
                            $crate::return_if_source!($attr_name, $attr_name $(<$($attr_type_param),*>)* )
                        );* )*;
                        None
                    }
                ),*}
            }
        }
        }}

        $crate::impl_error_conversion!{
            ( $errtype $(< $($type_param),* >)* )
            $([
                $field,
                $($(
                    $attr_name,
                    $attr_name,
                    $($attr_type)::* $(< $($attr_type_param),* >)*
                ),*),*
            ])*
        }

        $crate::add_type_bounds! {
        ( $($($type_param),*)* )
        (std::string::ToString)
        { impl <} {> std::fmt::Display
            for $errtype $( < $($type_param),* > )*
        {
            fn fmt(&self, formatter: &mut std::fmt::Formatter)
                -> std::fmt::Result
            {
                match self {$(
                    $errtype::$field $( { $( $attr_name ),* } )* => {
                        $(write!(formatter, "{}", ($($msg_fun)*) )?;)*
                        $crate::display_message!(formatter, $($($attr_name),*),* | $($msg)*);
                        Ok(())
                    }
                ),*}
            }
        }
        }}
    };
    (
        $( ($prefix:tt) )* // `pub` marker
        $errtype:ident // Name of the error type to generate
        $( < $(
            $type_param:tt // Optional type parameters for generic error types
            ),*
        > )*
        { $(
            $field_name:ident // Name of an attribute of the error variant
            :
            $($field_type:ident)::* // type of the attribute
            $(< $($field_type_param:tt),* >)* // Generic (lifetime & type) parameters for the attribute's type
        ),* }
        =
        $( @{ $($msg_fun:tt)* } )*
        $($msg:expr)* // The human-readable error message
        $(,)* // Trailing comma
    ) => {
        #[derive(Debug)]
        $($prefix)* struct $errtype $( < $($type_param),* > )* {
            $( $field_name : $($field_type)::* $(< $($field_type_param),* >)* ),*
        }

        $crate::add_type_bounds! {
        ( $($($type_param),*)* )
        (std::fmt::Debug + std::fmt::Display)
        { impl <} {> std::error::Error
            for $errtype $( < $($type_param),* > )*
        {
            #[allow(unused_variables, unreachable_code)]
            fn source(&self) -> Option<&(dyn std::error::Error + 'static)>
            {
                $(
                    $crate::return_if_source!(self, $field_name, $field_name $(<$($field_type_param),*>)* );
                );*
                None
            }
        }
        }}

        $crate::add_type_bounds! {
        ( $($($type_param),*)* )
        (std::string::ToString)
        { impl <} {> std::fmt::Display
            for $errtype $( < $($type_param),* > )*
        {
            fn fmt(&self, formatter: &mut std::fmt::Formatter)
                -> std::fmt::Result
            {
                // make fields accessible with variables, so that we can
                // use them in custom error msg blocks without self
                $(
                    let $field_name = &self.$field_name;
                );*
                $(write!(formatter, "{}", ($($msg_fun)*) )?;)*
                $crate::display_message!(formatter, $($field_name),* | $($msg)*);
                Ok(())
            }
        }
        }}
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! return_if_source {
    // Return the source if the attribute is called 'source'
    (source, $attr_name:ident) => { {return Some($attr_name)} };
    ($self:ident, source, $attr_name:ident) => { {return Some(&$self.$attr_name)} };
    // If the attribute has a different name or has type parameters, return nothing
    ($_attr_name:ident, $_repeat:ident $(<$($_type:tt),*>)* ) => { };
    ($self:ident, $_attr_name:ident, $_repeat:ident $(<$($_type:tt),*>)* ) => { };
}

#[doc(hidden)]
#[macro_export]
macro_rules! impl_error_conversion {
    ( ( $($prefix:tt)* ) [ $($field_data:tt)* ] $($rest:tt)* ) => {
        $crate::impl_error_conversion!{$($prefix)*, $($field_data)*}
        $crate::impl_error_conversion!{ ($($prefix)*) $($rest)*}
    };
    ( ( $($prefix:tt)* ) ) => {};
    // implement From<Source> only when there is a single attribute and it is named 'source'
    (
        $errtype:ident $( < $($type_param:tt),* > )*,
        $field:ident,
        source,
        $source:ident,
        $($source_type:ident)::* $( < $($source_type_param:tt),* > )*
    ) => {
        impl $( < $($type_param),* > )*
            From<$($source_type)::* $( < $($source_type_param),* > )*>
        for $errtype $( < $($type_param),* > )* {
            fn from(source: $($source_type)::* $( < $($source_type_param),* > )*) -> Self {
                $errtype::$field { source }
            }
        }
    };
    (
        $_errtype:ident $( < $($_errtype_type_param:tt),* > )*,
        $_field:ident,
        $(
            $_:ident,
            $_repeated:ident,
            $($_type:ident)::* $( < $($_type_param:tt),* > )*
        ),*
    ) => {}; // If the list of fields is not a single field named 'source', do nothing
}

#[doc(hidden)]
#[macro_export]
macro_rules! display_message {
    ($formatter:expr, $($attr:ident),* | $msg:expr) => {
        write!(
            $formatter,
            concat!($msg $(, "{", stringify!($attr), ":.0}" )*)
            $( , $attr = $attr.to_string() )*
        )?;
    };
    ($formatter:expr, $($attr:ident),* | ) => {};
}

/* This macro, given a list of generic parameters and type
bounds, adds the type bounds to all generic type parameters
(and leaves the generic lifetime parameters unchanged) */
#[doc(hidden)]
#[macro_export]
macro_rules! add_type_bounds {
    (
        ( $typ:ident $(, $rest:tt)* ) // type parameter
        ( $($bounds:tt)* )
        { $($prefix:tt)* }
        { $($suffix:tt)* }
    ) => {
        add_type_bounds!{
            ( $(, $rest)* )
            ( $($bounds)* )
            { $($prefix)* $typ : $($bounds)*}
            { $($suffix)* }
        }
    };
    (
        ( $lifetime:tt $(, $rest:tt)* ) // lifetime parameter
        ( $($bounds:tt)* )
        { $($prefix:tt)* }
        { $($suffix:tt)* }
    ) => {
        add_type_bounds!{
            ( $(, $rest)* )
            ( $($bounds)* )
            { $($prefix)* $lifetime }
            { $($suffix)* }
        }
    };
    (
        ( , $($rest:tt)* ) // add the comma to the prefix
        ( $($bounds:tt)* )
        { $($prefix:tt)* }
        { $($suffix:tt)* }
    ) => {
        add_type_bounds!{
            ( $($rest)* )
            ( $($bounds)* )
            { $($prefix)* , }
            { $($suffix)* }
        }
    };
    (
        (  ) // no more generic params to consume
        ( $($bounds:tt)* )
        { $($prefix:tt)* }
        { $($suffix:tt)* }
    ) => {
        $($prefix)* $($suffix)*
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn single_error_case() {
        custom_error!(MyError Bad="bad");
        assert_eq!("bad", MyError::Bad.to_string());
    }

    #[test]
    fn single_error_struct_case() {
        custom_error!(MyError{} ="bad");
        assert_eq!("bad", MyError{}.to_string());
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
    fn struct_with_error_data() {
        custom_error!(MyError{broken_things:u8} = "{broken_things} things are broken");
        assert_eq!("9 things are broken", MyError{ broken_things: 9 }.to_string());
    }

    #[test]
    fn with_multiple_error_data() {
        custom_error!(E X{a:u8, b:u8, c:u8} = "{c} {b} {a}");

        assert_eq!("3 2 1", E::X { a: 1, b: 2, c: 3 }.to_string());
    }

    #[test]
    fn struct_with_multiple_error_data() {
        custom_error!(E{a:u8, b:u8, c:u8} = "{c} {b} {a}");

        assert_eq!("3 2 1", E{ a: 1, b: 2, c: 3 }.to_string());
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
    fn struct_source() {
        use std::{io, error::Error};
        custom_error!(E{source: io::Error}="");
        let source: io::Error = io::ErrorKind::InvalidData.into();
        assert_eq!(
            source.to_string(),
            E{ source }.source().unwrap().to_string()
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

    #[test]
    fn pub_error() {
        mod my_mod {
            custom_error! {pub MyError Case1="case1"}
        }
        assert_eq!("case1", my_mod::MyError::Case1.to_string());
    }

    #[test]
    fn pub_error_struct() {
        mod my_mod {
            custom_error! {pub MyError{} = "case1"}
        }
        assert_eq!("case1", my_mod::MyError{}.to_string());
    }

    #[test]
    fn generic_error() {
        custom_error! {MyError<X,Y> E1{x:X,y:Y}="x={x} y={y}", E2="e2"}
        assert_eq!("x=42 y=42", MyError::E1 { x: 42u8, y: 42u8 }.to_string());
        assert_eq!("e2", MyError::E2::<u8, u8>.to_string());
    }

    #[test]
    fn generic_error_struct() {
        custom_error! {MyError<X,Y>{x:X,y:Y}="x={x} y={y}"}
        assert_eq!("x=42 y=42", MyError{ x: 42u8, y: 42u8 }.to_string());
    }

    #[test]
    fn single_error_case_with_braces() {
        custom_error! {MyError Bad="bad"}
        assert_eq!("bad", MyError::Bad.to_string());
    }

    #[test]
    fn single_error_struct_case_with_braces() {
        custom_error! {MyError{} ="bad"}
        assert_eq!("bad", MyError{}.to_string())
    }

    #[test]
    fn trailing_comma() {
        custom_error! {MyError1 A="a",}
        custom_error! {MyError2 A="a", B="b",}

        assert_eq!("a", MyError1::A.to_string());
        assert_eq!("a", MyError2::A.to_string());
        assert_eq!("b", MyError2::B.to_string());
    }

    #[test]
    fn with_custom_formatting() {
        custom_error! {MyError
            Complex{a:u8, b:u8} = @{
                if a+b == 0 {
                    "zero".to_string()
                } else {
                    (a+b).to_string()
                }
            },
            Simple = "simple"
        }

        assert_eq!("zero", MyError::Complex { a: 0, b: 0 }.to_string());
        assert_eq!("3", MyError::Complex { a: 2, b: 1 }.to_string());
        assert_eq!("simple", MyError::Simple.to_string());
    }

    #[test]
    fn struct_with_custom_formatting() {
        custom_error! {MyError{a:u8, b:u8} = @{
                if a+b == 0 {
                    "zero".to_string()
                } else {
                    (a+b).to_string()
                }
            }
        }

        assert_eq!("zero", MyError { a: 0, b: 0 }.to_string());
        assert_eq!("3", MyError { a: 2, b: 1 }.to_string());
    }

    #[test]
    fn custom_format_source() {
        use std::io;

        custom_error! {MyError
            Io{source:io::Error} = @{format!("IO Error occurred: {:?}", source.kind())}
        }

        assert_eq!(
            "IO Error occurred: Interrupted",
            MyError::Io { source: io::ErrorKind::Interrupted.into() }.to_string()
        )
    }

    #[test]
    fn struct_custom_format_source() {
        use std::io;

        custom_error! {MyError{source:io::Error} = @{format!("IO Error occurred: {:?}", source.kind())} }

        assert_eq!(
            "IO Error occurred: Interrupted",
            MyError { source: io::ErrorKind::Interrupted.into() }.to_string()
        )
    }

    #[test]
    fn lifetime_source_param() {

        #[derive(Debug)]
        struct SourceError<'my_lifetime> { x : &'my_lifetime str }
        impl<'a> std::fmt::Display for SourceError<'a> {
            fn fmt(&self, _: &mut std::fmt::Formatter) -> std::fmt::Result {
                Ok(())
            }
        }
        impl<'a> std::error::Error for SourceError<'a> {}

        custom_error! { MyError<'source_lifetime>
            Sourced { source : SourceError<'source_lifetime> } = @{ source.x },
            Other { source: std::fmt::Error } = "other error"
        }

        let sourced = MyError::Sourced { source : SourceError { x: "I am the source"} };
        assert_eq!("I am the source", sourced.to_string());
        let other_err : MyError = std::fmt::Error.into();
        assert_eq!("other error", other_err.to_string());

    }

    #[test]
    fn lifetime_param_and_type_param() {
        #[derive(Debug)]
        struct MyType<'a,T> {data: &'a str, _y: T}
        custom_error! { MyError<'a,T>
            X { d: MyType<'a,T> } = @{ format!("error x: {}", d.data) },
            Y { d: T } = "error y"
        }
        let err = MyError::X { d : MyType { data: "hello", _y:42i8 } };
        assert_eq!("error x: hello", err.to_string());
        let err_y = MyError::Y { d : String::from("my string") };
        assert_eq!("error y", err_y.to_string());
    }
}
