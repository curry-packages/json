------------------------------------------------------------------------------
-- | Author:  Jonas Oberschweiber
--   Version: February 2025
--
-- This library contains the definition of a data type to represent
-- JSON values.
------------------------------------------------------------------------------

module JSON.Data
  ( JValue(..)
  , JObject, fromJObject, toJObject, lookupName, insertField
  ) where

import Data.List ( nubBy, partition )

-- | Abstract representation of a JSON value.
data JValue =
    JBool   Bool    -- ^ a Boolean value (`true` or `false` in JSON)
  | JNull           -- ^ null, i.e. a missing value
  | JString String  -- ^ a JSON string
  | JInt    Int     -- ^ a JSON number without decimal point and exponent
  | JNumber Float   -- ^ a JSON number (numbers are always floats in JSON)
  | JArray [JValue] -- ^ a JSON array, represented by a list of JValues
  | JObject JObject -- ^ a JSON object, represented by a map
                    --   from Strings to JValues
  deriving (Eq, Show)

-- | A JSON object is just some representation of a mapping from names
--   (strings) to JSON values.
--   It is an abstract type (rather than an explicit list) to ensure
--   that there are no duplicate entries.
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

-- | Extracts the list of name / JSON value pairs from a JSON object.
fromJObject :: JObject -> [(String, JValue)]
fromJObject (JSONObject jo) = jo

-- | Transforms a list of name / JSON value pairs into a JSON object.
--   Pairs with duplicated names are deleted to ensure that the JSON object
--   is a map from names to values.
toJObject :: [(String,JValue)] -> JObject
toJObject = JSONObject . nubBy (\(k1, _) (k2, _) -> k1 == k2)

-- | Retrieves the JSON value with a given name from a JSON object if it exists.
lookupName :: String -> JObject -> Maybe JValue
lookupName name (JSONObject jo) = lookup name jo

-- | Inserts a name / JSON value pair in a JSON object.
--   If the name already exists, the existing value is overwritten.
insertField :: String -> JValue -> JObject -> JObject
insertField name val (JSONObject jo) = JSONObject (insert jo)
 where
  insert []                     = [(name,val)]
  insert ((k,v):fs) | k == name = (name,val) : fs
                    | otherwise = (k,v) : insert fs

------------------------------------------------------------------------------
