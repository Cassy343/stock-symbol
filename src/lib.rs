#![deny(unsafe_op_in_unsafe_fn, rust_2018_idioms)]
#![warn(missing_docs)]
#![doc = include_str!("../README.md")]

use std::{
    cmp::Ordering,
    error::Error,
    fmt::{self, Debug, Display, Formatter},
    num::NonZeroU64,
    ops::Deref,
    ptr, str,
};

/// A stock symbol, or ticker.
#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol {
    data: NonZeroU64,
}

impl Symbol {
    /// Converts the given `str` to a `Symbol`, returning an error if the provided string is an
    /// invalid length.
    ///
    /// Symbols must contain 7 or fewer characters, and must not be empty. If the provided string
    /// does not meet these criteria then [`InvalidSymbol`](crate::InvalidSymbol) is returned
    /// within the error variant.
    pub fn from_str<T: AsRef<str>>(symbol: T) -> Result<Self, InvalidSymbol> {
        #[inline]
        fn from_str_inner(symbol: &str) -> Result<Symbol, InvalidSymbol> {
            let len = symbol.len();

            if len.wrapping_sub(1) < 7 {
                // SAFETY: the check above asserts that the string is non-empty (else the
                // subtraction would overflow making the value greater than 7), and contains
                // fewer than eight bytes (since we subtract 1, we check this by ensuring the
                // altered value is less than 7).
                Ok(unsafe { Symbol::from_str_unchecked(symbol) })
            } else {
                Err(InvalidSymbol)
            }
        }

        from_str_inner(symbol.as_ref())
    }

    /// Converts the given string to a `Symbol` without a length check. This is equivalent to
    /// `Symbol::from_bytes_unchecked(symbol.as_bytes())`.
    ///
    /// # Safety
    ///
    /// The given `str` must not be empty, and must contain fewer than eight bytes.
    #[inline]
    pub unsafe fn from_str_unchecked(symbol: &str) -> Self {
        // SAFETY: the length requirement is upheld by the caller, and since the bytes are
        // obtained from a `str`, they are valid UTF-8.
        unsafe { Self::from_bytes_unchecked(symbol.as_bytes()) }
    }

    /// Converts the given slice to a `Symbol` without a length check.
    ///
    /// # Safety
    ///
    /// The given slice must not be empty, and must contain fewer than eight bytes. Additionally,
    /// the slice must satisfy the safety conditions of
    /// [`from_utf8_unchecked`](std::str::from_utf8_unchecked).
    #[inline]
    pub unsafe fn from_bytes_unchecked(symbol: &[u8]) -> Self {
        let mut bytes = [0u8; 8];
        let len = symbol.len();
        bytes[7] = len as u8;

        // SAFETY: the caller ensures `symbol` contains fewer than 8 bytes, so `bytes` is valid for
        // writes of `len` bytes. Moreover, since `len` is the length of `symbol`, `symbol` is
        // valid for reads of `len` bytes. The source and destination are trivially
        // non-overlapping, and trivially aligned because they were obtained from valid references.
        unsafe { ptr::copy_nonoverlapping(symbol.as_ptr(), bytes.as_mut_ptr(), len) };

        Self {
            // SAFETY: since the caller ensures the lenth is not zero, we know that byte within
            // the integer is not zero, so the integer overall is non-zero.
            data: unsafe { NonZeroU64::new_unchecked(u64::from_ne_bytes(bytes)) },
        }
    }

    /// Returns a `&str` representing this symbol as a string.
    ///
    /// This operation is not a no-op, but is very cheap. The return value will compare equal
    /// to the string this symbol was constructed with.
    #[inline]
    pub fn as_str(&self) -> &str {
        // SAFETY: [u8; 8] and NonZeroU64 have the same size. [u8; 8] is a POD type, and NonZeroU64
        // does not contain any uninitialized bytes, so it is safe to cast an immutable reference
        // from the latter to the former (see bytemuck). Moreover, NonZeroU64 has an alignment of
        // 8, and [u8; 8] has an alignment of 1, so the created reference is properly aligned.
        let bytes = unsafe { &*(&self.data as *const NonZeroU64 as *const [u8; 8]) };

        let len = usize::from(bytes[7]);

        // SAFETY: since the only way to (safely) construct a value of this type involves ensuring
        // its length is less than 8, we know the length is in bounds.
        let str_bytes = unsafe { bytes.get_unchecked(..len) };

        // SAFETY: to (safely) construct a value of this type, the caller must assert that the
        // bytes forming the symbol contain valid UTF-8, so the safety conditions of this function
        // are satisfied.
        unsafe { str::from_utf8_unchecked(str_bytes) }
    }
}

impl Debug for Symbol {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Symbol({})", self.as_str())
    }
}

impl Display for Symbol {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.pad(self.as_str())
    }
}

impl AsRef<str> for Symbol {
    #[inline]
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl Deref for Symbol {
    type Target = str;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl PartialEq<str> for Symbol {
    #[inline]
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl PartialEq<Symbol> for str {
    #[inline]
    fn eq(&self, other: &Symbol) -> bool {
        other == self
    }
}

impl PartialEq<&str> for Symbol {
    #[inline]
    fn eq(&self, other: &&str) -> bool {
        self == *other
    }
}

impl PartialEq<Symbol> for &str {
    #[inline]
    fn eq(&self, other: &Symbol) -> bool {
        *self == other
    }
}

impl PartialEq<String> for Symbol {
    #[inline]
    fn eq(&self, other: &String) -> bool {
        self == &**other
    }
}

impl PartialEq<Symbol> for String {
    #[inline]
    fn eq(&self, other: &Symbol) -> bool {
        self.as_str() == other
    }
}

impl PartialOrd for Symbol {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let this = u64::from_be(self.data.get());
        let other = u64::from_be(other.data.get());
        this.partial_cmp(&other)
    }
}

impl Ord for Symbol {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        let this = u64::from_be(self.data.get());
        let other = u64::from_be(other.data.get());
        this.cmp(&other)
    }
}

/// An error signifying an invalid symbol was encountered when parsing. See
/// `Symbol::`[`from_str`](crate::Symbol::from_str) for details.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InvalidSymbol;

impl Display for InvalidSymbol {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("Invalid symbol length; length must be greater than 0 and less than 8")
    }
}

impl Error for InvalidSymbol {}

#[cfg(feature = "serde")]
mod serde {
    use super::*;
    use ::serde::{
        de::{self, Visitor},
        Deserialize, Deserializer, Serialize, Serializer,
    };

    impl Serialize for Symbol {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            serializer.serialize_str(self.as_str())
        }
    }

    impl<'de> Deserialize<'de> for Symbol {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            deserializer.deserialize_str(SymbolVisitor)
        }
    }

    struct SymbolVisitor;

    impl<'de> Visitor<'de> for SymbolVisitor {
        type Value = Symbol;

        #[inline]
        fn expecting(&self, f: &mut Formatter<'_>) -> fmt::Result {
            f.write_str("A symbol string")
        }

        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Symbol::from_str(v).map_err(E::custom)
        }
    }
}

#[cfg(feature = "sqlx")]
mod sqlx {
    use super::*;
    use sqlx_core::{
        database::{Database, HasValueRef},
        decode::Decode,
        error::BoxDynError,
        types::Type,
    };

    impl<'r, DB: Database> Decode<'r, DB> for Symbol
    where
        &'r str: Decode<'r, DB>,
    {
        fn decode(value: <DB as HasValueRef<'r>>::ValueRef) -> Result<Self, BoxDynError> {
            let value = <&str as Decode<DB>>::decode(value)?;
            Self::from_str(value).map_err(Into::into)
        }
    }

    impl<DB: Database> Type<DB> for Symbol
    where
        str: Type<DB>,
    {
        fn type_info() -> <DB as Database>::TypeInfo {
            <str as Type<DB>>::type_info()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sizes() {
        use std::mem::size_of;

        assert_eq!(size_of::<Symbol>(), 8);
        assert_eq!(size_of::<Option<Symbol>>(), 8);
        assert_eq!(size_of::<Result<Symbol, InvalidSymbol>>(), 8);
    }

    #[test]
    fn creation() {
        assert!(Symbol::from_str("AAPL").is_ok());
        assert!(Symbol::from_str("HSBC.A").is_ok());
        assert!(Symbol::from_str("this is too long").is_err());
        assert!(Symbol::from_str("").is_err());
    }

    #[test]
    fn equality() {
        let sym1 = Symbol::from_str("ABCDEFG").unwrap();
        let sym2 = Symbol::from_str("ABCDEFG").unwrap();
        let sym3 = Symbol::from_str("ABCDEF").unwrap();

        assert_eq!(sym1, sym2);
        assert_ne!(sym1, sym3);
        assert_ne!(sym2, sym3);
    }

    #[test]
    fn str_equality() {
        let sym1 = Symbol::from_str("ABCDEFG").unwrap();
        let sym2 = Symbol::from_str("ABCDEF").unwrap();
        let str1 = "ABCDEFG";
        let str2 = "ABCDEF";

        assert_eq!(sym1, str1);
        assert_eq!(sym2, str2);
        assert_ne!(sym1, str2);
        assert_ne!(sym2, str1);
    }

    #[test]
    fn ord() {
        let symbols = ["A", "AA", "AB", "B", "BBB", "C", "CA", "CBS"]
            .map(|symbol| Symbol::from_str(symbol).unwrap());

        for (i, sym1) in symbols.into_iter().enumerate() {
            for (j, sym2) in symbols.into_iter().enumerate() {
                assert_eq!(i.cmp(&j), sym1.cmp(&sym2));
            }
        }
    }

    #[test]
    fn formatting() {
        let symbol = Symbol::from_str("FOO").unwrap();

        assert_eq!(format!("{symbol}"), "FOO");
        assert_eq!(format!("{symbol:<5}"), "FOO  ");
        assert_eq!(format!("{symbol:>5}"), "  FOO");
        assert_eq!(format!("{symbol:^5}"), " FOO ");
    }
}
