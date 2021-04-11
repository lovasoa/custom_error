//! The Error trait currently resides in std and is not available for use in no_std.
//!
//! We provide our own Error trait in this case and implement it for all errors in
//! `core` and `alloc`.
//!
//! Error might go back be in libcore again:
//! https://doc.rust-lang.org/nightly/src/std/error.rs.html#5
//!
//! Once this happens, this file will become obsolete and can be removed.

use core::any::TypeId;
use core::fmt::{Debug, Display};

/// A copy of the Error trait definition from libstd (for now).
pub trait Error: Debug + Display {
    /// Human-readable error description
    fn description(&self) -> &str {
        "description() is deprecated; use Display"
    }

    /// The lower-level source of this error, if any.
    fn cause(&self) -> Option<&dyn Error> {
        self.source()
    }

    /// The lower-level source of this error, if any.
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }

    /// type id
    fn type_id(&self) -> TypeId
    where
        Self: 'static,
    {
        TypeId::of::<Self>()
    }
}

impl Error for core::alloc::LayoutErr {}
impl Error for core::array::TryFromSliceError {}
impl Error for core::cell::BorrowError {}
impl Error for core::cell::BorrowMutError {}
impl Error for core::char::CharTryFromError {}
impl Error for core::char::DecodeUtf16Error {}
impl Error for core::char::ParseCharError {}
impl Error for core::fmt::Error {}
impl Error for core::num::ParseFloatError {}
impl Error for core::num::ParseIntError {}
impl Error for core::num::TryFromIntError {}
impl Error for core::str::ParseBoolError {}
impl Error for core::str::Utf8Error {}
impl Error for alloc::string::ParseError {}

#[cfg(feature = "unstable")]
impl Error for core::alloc::AllocError {}
#[cfg(feature = "unstable")]
impl Error for alloc::collections::TryReserveError {}
