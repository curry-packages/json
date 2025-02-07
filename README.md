# JSON

This package provides data types, a parser and pretty printer, and
conversion operations for [JSON][1].

## Representing JSON values in Curry

A JSON value can be a primitive, i.e. `true`, `false`, `null`, a string or a 
number, an array of JSON values or an object mapping strings to JSON values.
In Curry, a JSON value is represented by the data type `JValue` from the 
`JSON.Data` module:

```haskell
data JValue = JBool Bool
            | JNull
            | JString String
            | JInt Int
            | JNumber Float
            | JArray [JValue]
            | JObject JObject
```

`JObject` is an abstract type representing a mapping from names (strings)
to JSON values (see module `JSON.Data` for operations in this type).

In contrast to raw [JSON][1], this definition distinguishes between
integers and other numbers. Actually, the JSON parser (see below)
returns a number as a `JInt` if its textual representation does not
contain a decimal point and an exponent.

## Parsing JSON strings

`parseJSON` from `JSON.Parser` can be used to parse a JSON string into a 
`JValue`:

```haskell
> parseJSON "{ \"hello\": [\"world\", \"kiel\"] }"
Just (JObject [("hello", JArray [JString "world", JString "kiel"])])
```

## Printing JSON strings

`ppJSON` from `JSON.Pretty` will turn a `JValue` into a pretty printed string.
If you want more control over the layout of the resulting string, you can use
`ppJValue` from the same value to obtain a `Doc` for Curry's `Pretty` module 
from a `JValue`. 

## Converting to and from JSON

The module `JSON.Convert` defines a defines a type class `ConvertJSON`
and various instances for standard types to convert Curry values to JSON values
and vice versa.
As apparent from these instance definitions, it is quite easy to
define instances for other user-defined data types.

[1]: http://www.json.org
