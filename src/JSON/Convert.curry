-- | Author : Michael Hanus
--   Version: February 2025
--
-- This library defines a type class and instances for standard types
-- to convert Curry values to JSON values and vice versa.
------------------------------------------------------------------------------

module JSON.Convert where

import Data.Maybe ( catMaybes, isJust )
import JSON.Data

-- | The type class `ConvertJSON` defines conversion operations
--   between values and their JSON representation.
--   Since a JSON value might not contain a correct representation
--   of a standard value, the operation `fromJSON` returns a `Maybe` value.
--   The additional operations on value lists are used for a better
--   JSON conversion of strings.
class ConvertJSON a where
  -- | Convert a value into its JSON representation.
  toJSON :: a -> JValue

  -- | Convert a JSON representation into its corresponding value.
  fromJSON :: JValue -> Maybe a

  -- | Convert a list of values into its JSON rerpresentation.
  --   As a default, a JSON array is used to represent the list of values.
  toJSONList :: [a] -> JValue

  -- | Convert a JSON representation into the corresponding list of values.
  fromJSONList :: JValue -> Maybe [a]

  toJSONList = JArray . map toJSON

  fromJSONList jv = case jv of
    JArray xs -> let ys = map fromJSON xs
                 in if all isJust ys then Just (catMaybes ys)
                                     else Nothing
    _         -> Nothing

-- Instance for Booleans.
instance ConvertJSON Bool where
  toJSON b = JBool b

  fromJSON jv = case jv of
    JBool b -> Just b
    _       -> Nothing

-- Instance for characters and strings.
instance ConvertJSON Char where
  toJSON c = JString [c]

  fromJSON jv = case jv of
    JString [c] -> Just c
    _           -> Nothing

  toJSONList s = JString s

  fromJSONList jv = case jv of
    JString s -> Just s
    _         -> Nothing

-- Instance for floats.
instance ConvertJSON Float where
  toJSON x = JNumber x

  fromJSON jv = case jv of
    JNumber n -> Just n
    _         -> Nothing

-- Instance for integers.
instance ConvertJSON Int where
  toJSON n = JInt n

  fromJSON jv = case jv of
    JInt n -> Just n
    _      -> Nothing

-- Instance for lists.
instance ConvertJSON a => ConvertJSON [a] where
  toJSON = toJSONList

  fromJSON = fromJSONList

-- Instance for `Maybe` values.
instance ConvertJSON a => ConvertJSON (Maybe a) where
  toJSON Nothing  = JNull
  toJSON (Just x) = toJSON x

  fromJSON jv = case jv of
    JNull -> Just Nothing
    _     -> fmap Just (fromJSON jv)

-- Instance for `Either` values.
instance (ConvertJSON a, ConvertJSON b) => ConvertJSON (Either a b) where
  toJSON (Left  x) = JObject $ toJObject [("Left",  toJSON x)]
  toJSON (Right y) = JObject $ toJObject [("Right", toJSON y)]

  fromJSON jv = case jv of
    JObject jo -> case fromJObject jo of
                    [("Left", v)] -> fmap Left  (fromJSON v)
                    [("Right",v)] -> fmap Right (fromJSON v)
                    _             -> Nothing
    _          -> Nothing

-- Instance for `Ordering` values.
instance ConvertJSON Ordering where
  toJSON x = JString (show x)

  fromJSON jv = case jv of
    JString s -> case reads s of [(x,"")] -> Just x
                                 _        -> Nothing
    _         -> Nothing

-- Instance for pairs of values.
instance (ConvertJSON a, ConvertJSON b) => ConvertJSON (a,b) where
  toJSON (x,y) = JObject $ toJObject [("1", toJSON x), ("2", toJSON y)]

  fromJSON jv = case jv of
    JObject jo -> case fromJObject jo of
                    [("1",v1), ("2",v2)] -> do x <- fromJSON v1
                                               y <- fromJSON v2
                                               return (x,y)
                    _                    -> Nothing
    _          -> Nothing

------------------------------------------------------------------------------
