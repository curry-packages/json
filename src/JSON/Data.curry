------------------------------------------------------------------------------
--- This library contains the definition of a data type to represent
--- JSON values.
---
--- @author Jonas Oberschweiber
--- @version February 2025
------------------------------------------------------------------------------

module JSON.Data
  ( JValue(..)
  , JObject, fromJObject, toJObject, getField, setField
  ) where

import Data.List ( nubBy, partition )

--- A JSON value.
---
--- @cons JBool   - a Boolean value (`true` or `false` in JSON)
--- @cons JNull   - null, i.e. a missing value
--- @cons JString - a JSON string
--- @cons JInt    - a JSON number without decimal point and exponent
--- @cons JNumber - a JSON number (numbers are always floats in JSON)
--- @cons JArray  - a JSON array, represented by a list of JValues
--- @cons JObject - a JSON object, represented by a map from Strings to JValues
data JValue = JBool Bool
            | JNull
            | JString String
            | JInt Int
            | JNumber Float
            | JArray [JValue]
            | JObject JObject
  deriving (Eq, Show)

--- A JSON object is just some representation of a mapping from names (strings)
--- to JSON values.
--- It is an abstract type (rather than an explicit list) to ensure
--- that there are no duplicate entries.
newtype JObject = JSONObject [(String, JValue)]
  deriving Show

-- JSON objects are equivalent if they contain the same name/value pairs
-- possibly in a different order.
instance Eq JObject where
  JSONObject jo1 == JSONObject jo2 = eqJObject jo1 jo2
   where
    eqJObject []          []    = True
    eqJObject []          (_:_) = False
    eqJObject ((k,v):kvs) jo    =
      let (eqk,neqk) = partition ((==k) . fst) jo
      in case eqk of [(_,v1)] -> v==v1 && eqJObject kvs neqk
                     _        -> False

--- Extracts the list of name / JSON value pairs from a JSON object.
fromJObject :: JObject -> [(String, JValue)]
fromJObject (JSONObject jo) = jo

--- Transforms a list of name / JSON value pairs into a JSON object.
--- Pairs with duplicated names are deleted to ensure that the JSON object
--- is a map from names to values.
toJObject :: [(String,JValue)] -> JObject
toJObject = JSONObject . nubBy (\(k1, _) (k2, _) -> k1 == k2)

--- Gets the value with a given name from a JSON object, if it exists.
getField :: JObject -> String -> Maybe JValue
getField (JSONObject jo) n = lookup n jo

--- Sets the value associated to the given name in a JSON object.
--- If the name already exists, the existing value is overwritten.
setField :: JObject -> String -> JValue -> JObject
setField (JSONObject jo) name val = JSONObject (set jo)
 where
  set []                     = [(name,val)]
  set ((k,v):fs) | k == name = (name,val) : fs
                 | otherwise = (k,v) : set fs
                
------------------------------------------------------------------------------
