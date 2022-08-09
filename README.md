A data type for representing [stock symbols](https://www.investopedia.com/terms/s/stocksymbol.asp).

The data associated with a `Symbol` is allocated on the stack rather than the heap. In order to
accomodate this optimization, the length of a `Symbol` is limited to 7 characters. The
representation in memory also allows for highly optimized comparisons, exceeding the performance of
stack-allocated arrays. Note because of its optimized size, `Symbol` implements the `Copy` trait,
and should be passed by value rather than by reference.

`Symbol`s can be easily converted from and to `&str`s via [`from_str`](crate::Symbol::from_str) and
[`as_str`](crate::Symbol::as_str). For convenience, `Symbol` also implements `AsRef<str>`,
`Borrow<str>`, and `Deref<Target = str>`. Moreover, equality comparison against string types is
implemented for `Symbol` as well.

# Examples

```rust
use stock_symbol::Symbol;

// Make a new symbol
let symbol = Symbol::from_str("AAPL").unwrap();

// Symbols cannot be empty, and must contain fewer than 8 characters
assert!(Symbol::from_str("").is_err());
assert!(Symbol::from_str("12345678").is_err());

// Symbols implement Copy
let symbol_copy = symbol;
assert_eq!(symbol_copy, symbol);

// They can also be compared to strings...
assert_eq!(symbol, "AAPL");

// ...and easily converted into strings
let symbol_str: &str = symbol.as_str();
assert_eq!(symbol_str, "AAPL");

// Symbol also implements Ord and Hash for use in other data structures
let symbol2 = Symbol::from_str("BAC").unwrap();
assert!(symbol < symbol2);

let mut map = std::collections::HashMap::new();
map.insert(symbol, 123.0f64);
```

# Features

The `serde` feature enables serde support. `Symbol`s are currently serialized as, and deserialized
from strings. Other formats are unlikely to be supported in the future. If more direct control is
needed, then a custom serializer/deserializer can be made.

The `sqlx-*` features enables support for encoding and decoding `Symbol`s directly from `sqlx`
queries and fetch results. Similar to `serde`, `Symbol`s are encoded and decoded as `&str`s. `sqlx`
requires a runtime to be selected in order for the crate to compile, so the following separate
features correspond to `sqlx` runtimes, and you should pick one for the runetime you are using:
 - `sqlx-actix-native-tls`
 - `sqlx-async-std-native-tls`
 - `sqlx-tokio-native-tls`
 - `sqlx-actix-rustls`
 - `sqlx-async-std-rustls`
 - `sqlx-tokio-rustls`
