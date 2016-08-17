module JSON.Data (JValue (..)) where

--- A JSON value.
---
--- @cons JTrue - true
--- @cons JFalse - false
--- @cons JNull - null, i.e. a missing value
--- @cons JString - a JSON string
--- @cons JNumber - a JSON number (numbers are always floats in JSON)
--- @cons JArray - a JSON array, represented by a list of JValues
--- @cons JObject - a JSON object, represented by a map from Strings to JValues
data JValue = JTrue
            | JFalse
            | JNull
            | JString String
            | JNumber Float
            | JArray [JValue]
            | JObject [(String, JValue)]

