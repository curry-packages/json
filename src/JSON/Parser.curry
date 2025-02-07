------------------------------------------------------------------------------
--- This library contains the implementation of a parser for JSON values,
--- i.e., an operation `parseJSON` which reads a textual JSON representation
--- and returns a `Maybe` value of type `JValue`.
---
--- @author Jonas Oberschweiber
--- @version February 2025
------------------------------------------------------------------------------

module JSON.Parser ( parseJSON ) where

import JSON.Data
import Data.Char
import DetParse
import Test.Prop

import Prelude hiding (some, empty, (<|>), (<$>), (<*>), (<*), (*>))

--- Parses a JSON string into a JValue.
--- Returns `Nothing` if the string could not be parsed.
parseJSON :: String -> Maybe JValue
parseJSON = parse (pWhitespace *> pJValue)

--- Parser for a JValue with whitespace at the end.
pJValue :: Parser JValue
pJValue = ( pTrue
        <!> pFalse
        <!> pNull
        <!> pJString
        <!> pJNumber
        <!> pArray
        <!> pObject ) <* pWhitespace

pObject :: Parser JValue
pObject = char '{' *> pWhitespace *> pObjectNB

pObjectNB :: Parser JValue
pObjectNB =
  JObject . toJObject
  <$> ((char '}' *> yield []) <!> (pObjectFields <* char '}'))

pObjectFields :: Parser [(String, JValue)]
pObjectFields = (:) <$>
  pKeyValuePair <*> (char ',' *> pWhitespace *> pObjectFields <!> yield [])

pKeyValuePair :: Parser (String, JValue)
pKeyValuePair =
  (,) <$> pString <*> (pWhitespace *> char ':' *> pWhitespace *> pJValue)

test_pObject_empty :: Prop
test_pObject_empty = parse pObject "{}" -=- Just (JObject $ toJObject [])

test_pObject_onlyStringKeys :: Prop
test_pObject_onlyStringKeys = parse pObject "{1: 2}" -=- Nothing

test_pObject_simple :: Prop
test_pObject_simple =
  parse pObject "{\"test\": 1, \"test2\": false}"
  -=- Just (JObject $ toJObject [("test", JInt 1), ("test2", JBool False)])

test_pObject_whitespace :: Prop
test_pObject_whitespace =
  parse pObject "{\n \"test\": 1,\n \"test2\": false\n}"
  -=- Just (JObject $ toJObject [("test", JInt 1), ("test2", JBool False)])

test_pObject_nested :: Prop
test_pObject_nested =
  parse pObject "{\"test\": {\"hello\": \"world\"}}" -=-
  Just (JObject $ toJObject
          [("test", JObject $ toJObject [("hello", JString "world")])])

pArray :: Parser JValue
pArray = char '[' *> pWhitespace *> pArrayNB

pArrayNB :: Parser JValue
pArrayNB =  JArray <$> ((char ']' *> yield []) <!> (pArrayElems <* char ']'))

pArrayElems :: Parser [JValue]
pArrayElems = (:) <$>
  pJValue <*> ((char ',' *> pWhitespace *> pArrayElems) <!> yield [])

test_pArray_empty :: Prop
test_pArray_empty = parse pArray "[]" -=- Just (JArray [])

test_pArray_single1 :: Prop
test_pArray_single1 = parse pArray "[1]" -=- Just (JArray [JInt 1])

test_pArray_single2 :: Prop
test_pArray_single2 = parse pArray "[2.0]" -=- Just (JArray [JNumber 2.0])

test_pArray_multi :: Prop
test_pArray_multi =
  parse pArray "[true, false, null]"
  -=- Just (JArray [JBool True, JBool False, JNull])

test_pArray_nested :: Prop
test_pArray_nested =
  parse pArray "[true, [false], [[null]]]"
  -=- Just (JArray [JBool True, JArray [JBool False], JArray [JArray [JNull]]])

{-
-- Definition with parser combinators:
pWhitespace' :: Parser ()
pWhitespace' =   char ' ' *> pWhitespace
            <!> char '\n' *> pWhitespace
            <!> char '\r' *> pWhitespace
            <!> char '\t' *> pWhitespace
            <!> empty
-}
-- Direct definition without parser combinators (a bit faster):
pWhitespace :: Parser ()
pWhitespace [] = [((),"")]
pWhitespace s@(c:cs) | c `elem` [' ','\n','\r','\t'] = pWhitespace cs
                     | otherwise                     = [((),s)]

pTrue :: Parser JValue
pTrue = word "true" *> yield (JBool True)

pFalse :: Parser JValue
pFalse = word "false" *> yield (JBool False)

pNull :: Parser JValue
pNull = word "null" *> yield JNull

pJString :: Parser JValue
pJString = JString <$> pString

pString :: Parser String
pString = char '"' *> pCharSequence <* char '"'

pCharSequence :: Parser String
pCharSequence =
      (++) <$> (char '\\' *> (pEscaped <!> failure)) <*> pCharSequence
  <!> (:) <$> check (\c -> c /= '"' && c /= '\\') anyChar <*> pCharSequence
  <!> yield ""

pEscaped :: Parser String
pEscaped =   char '"'  *> yield "\""
         <!> char '\\' *> yield "\\"
         <!> char '/'  *> yield "/"
         <!> char 'b'  *> yield "\b"
         <!> char 'f'  *> yield "\f"
         <!> char 'n'  *> yield "\n"
         <!> char 'r'  *> yield "\r"
         <!> char 't'  *> yield "\t"
         <!> ((:[]) . chr) <$> (char 'u' *> pTwoByteHex)

pTwoByteHex :: Parser Int
pTwoByteHex =
  hexToInt <$>
  ((:) <$> pHexDigit <*> ((:) <$> pHexDigit <*> ((:)
       <$> pHexDigit <*> ((:[]) <$> pHexDigit))))
 where pHexDigit = check isHexDigit anyChar

hexToInt :: String -> Int
hexToInt s = foldl1 ((+) . (16*)) (map digitToInt s)

test_pCharSequence_simple :: Prop
test_pCharSequence_simple = parse pCharSequence "test" -=- Just "test"

test_pCharSequence_noDoubleQuote :: Prop
test_pCharSequence_noDoubleQuote = parse pCharSequence "te\"st" -=- Nothing

test_pCharSequence_noStandaloneBackslash :: Prop
test_pCharSequence_noStandaloneBackslash =
  parse pCharSequence "He\\world" -=- Nothing

test_pCharSequence_escapedDoubleQuote :: Prop
test_pCharSequence_escapedDoubleQuote =
  parse pCharSequence "Hello \\\"World\\\"" -=- Just "Hello \"World\""

test_pCharSequence_escapedBackslash :: Prop
test_pCharSequence_escapedBackslash =
  parse pCharSequence "He\\\\world" -=- Just "He\\world"

test_pCharSequence_escapedSlash :: Prop
test_pCharSequence_escapedSlash =
  parse pCharSequence "He\\/world" -=- Just "He/world"

test_pCharSequence_escapedBackspace :: Prop
test_pCharSequence_escapedBackspace =
  parse pCharSequence "He\\bworld" -=- Just "He\bworld"

test_pCharSequence_escapedFormFeed :: Prop
test_pCharSequence_escapedFormFeed =
  parse pCharSequence "He\\fworld" -=- Just "He\fworld"

test_pCharSequence_escapedNewline :: Prop
test_pCharSequence_escapedNewline =
  parse pCharSequence "He\\nworld" -=- Just "He\nworld"

test_pCharSequence_escapedCarriageReturn :: Prop
test_pCharSequence_escapedCarriageReturn =
  parse pCharSequence "He\\rworld" -=- Just "He\rworld"

test_pCharSequence_escapedTab :: Prop
test_pCharSequence_escapedTab =
  parse pCharSequence "He\\tworld" -=- Just "He\tworld"

test_pCharSequence_twoEscapes :: Prop
test_pCharSequence_twoEscapes =
  parse pCharSequence "He\\r\\nWorld" -=- Just "He\r\nWorld"

test_pCharSequence_escapedUnicodeChar :: Prop
test_pCharSequence_escapedUnicodeChar = 
  parse pCharSequence "Hello \\u2603 World" -=- Just "Hello ☃ World"

test_pCharSequence_escapedUnicodeRequiresFourDigits :: Prop
test_pCharSequence_escapedUnicodeRequiresFourDigits =
  parse pCharSequence "Hello \\u26 World" -=- Nothing

test_pString_simple :: Prop
test_pString_simple =
  parse pString "\"Hello, World\"" -=- Just "Hello, World"

test_pString_complex :: Prop
test_pString_complex =
  parse pString "\"Hello \\r\\n \\u2603 World\"" -=- Just "Hello \r\n ☃ World"

pJNumber :: Parser JValue
pJNumber =  (char '-' *> pPositiveFloat True)
        <!> pPositiveFloat False

-- parse a number with optional decimal point and option exponent
pPositiveFloat :: Bool -> Parser JValue
pPositiveFloat neg = (uncurry toJNum) <$> pWithOptDecimalPoint <*> pExponent
 where
  toJNum n Nothing  Nothing  = JInt (if neg then negate n else n)
  toJNum n Nothing  (Just e) = toJNumber (fromInt n * (exp 10 e))
  toJNum n (Just d) Nothing  = toJNumber (fromInt n * (exp 10 d))
  toJNum n (Just d) (Just e) = toJNumber ((fromInt n) * (exp 10 (d + e)))

  toJNumber x = JNumber (if neg then negate x else x)
  exp x y = if y < 0 then 1 / (x ^ (0 - y)) else (x ^ y)


pExponent :: Parser (Maybe Int)
pExponent =
      (char 'e' <!> char 'E') *>
       (Just <$>
         ((char '-' *> yield negate <!> char '+' *> yield id <!> yield id)
          <*> pInt))
  <!> yield Nothing

pWithOptDecimalPoint :: Parser (Int, Maybe Int)
pWithOptDecimalPoint =
  combine <$> some pDigit <*> (char '.' *> some pDigit <!> yield "")
 where
  s2i cs = foldl1 ((+).(10*)) (map (\c' -> ord c' - ord '0') cs)
  combine n d = (s2i (n ++ d),
                 if null d then Nothing else Just (negate $ length d))

pInt :: Parser Int
pInt =
  (\cs -> foldl1 ((+).(10*)) (map (\c' -> ord c' - ord '0') cs)) <$> some pDigit

pDigit :: Parser Char
pDigit = check isDigit anyChar
