-- Some simple tests for the JSON package.

import Data.Maybe
import Test.Prop

import JSON.Data
import JSON.Parser
import JSON.Pretty

-- Equality of JSON values where float equality is approximated.
eqJValue :: JValue -> JValue -> Bool
eqJValue jv1 jv2 = case (jv1,jv2) of
  (JNumber x, JNumber y)  -> abs (x - y) < 0.0001
  (JArray xs, JArray ys)  -> all (uncurry eqJValue) (zip xs ys)
  (JObject xs,JObject ys) -> all eqComp (zip (fromJObject xs) (fromJObject ys))
  _                       -> jv1 == jv2
 where
  eqComp ((s,x),(t,y)) = s == t && eqJValue x y

-- A simple JSON value.
jsonDoc :: JValue
jsonDoc =
  JObject $ toJObject [("hello", JArray [JString "world", JString "kiel"])]

-- Parsing JSON strings.
testParse :: Prop
testParse = parseJSON "{ \"hello\": [\"world\", \"kiel\"] }" -=- Just jsonDoc

-- Parsing JSON integers.
testParseInt :: Int -> Prop
testParseInt n = parseJSON (show n) -=- Just (JInt n)

-- Parsing JSON numbers.
testParseFloat1 :: Prop
testParseFloat1 = parseJSON "3.14" -=- Just (JNumber 3.14)

-- Parsing JSON numbers.
testParseFloat2 :: Prop
testParseFloat2 = parseJSON "314e-2" -=- Just (JNumber 3.14)

-- Check whether pretty printing and parsing of a JSON value yields same value.
eqParsePrint :: JValue -> Bool
eqParsePrint json = eqJValue (fromJust (parseJSON (ppJSON json))) json

-- Test property of pretty printing and parsing a JSON document.
testParsePrint :: Prop
testParsePrint = always $ eqParsePrint jsonDoc

-- Test property of pretty printing and parsing a JSON integer array.
testParsePrintIArray :: Int -> Int -> Prop
testParsePrintIArray i1 i2 = always $ eqParsePrint (JArray [JInt i1, JInt i2])

-- Test field setting.
testSetField :: Int -> Prop
testSetField i =
  getField (setField (toJObject []) "value" (JInt i)) "value" -=- Just (JInt i)
