module JSON.Parser (parseJSON) where

import Prelude hiding ((<$>))
import JSON.Data
import Data.Char
import DetParse
import Test.EasyCheck

--- Parses a JSON string into a JValue. Returns Nothing if the string could not
--- be parsed.
parseJSON :: String -> Maybe JValue
parseJSON = parse pJValue

--- Parser for a JValue
pJValue :: Parser JValue
pJValue =   pTrue
        <|> pFalse
        <|> pNull
        <|> pJString
        <|> pJNumber
        <|> pArray
        <|> pObject

pObject :: Parser JValue
pObject =   JObject <$> (char '{' *> pWhitespace *> pObject' <* pWhitespace <* char '}' <* pWhitespace)
        <|> JObject <$> (char '{' *> pWhitespace *> char '}' *> yield [])

pObject' :: Parser [(String, JValue)]
pObject' = (:) <$> (pWhitespace *> pKeyValuePair) <*> (pWhitespace *> char ',' *> pObject' <|> yield [])

pKeyValuePair :: Parser (String, JValue)
pKeyValuePair = (,) <$> pString <*> (pWhitespace *> char ':' *> pWhitespace *> pJValue)

test_pObject_empty :: Test.EasyCheck.Prop
test_pObject_empty = parse pObject "{}" -=- Just (JObject [])

test_pObject_onlyStringKeys :: Test.EasyCheck.Prop
test_pObject_onlyStringKeys = parse pObject "{1: 2}" -=- Nothing

test_pObject_simple :: Test.EasyCheck.Prop
test_pObject_simple = parse pObject "{\"test\": 1, \"test2\": false}" -=- Just (JObject [("test", JNumber 1.0), ("test2", JFalse)])

test_pObject_whitespace :: Test.EasyCheck.Prop
test_pObject_whitespace = parse pObject "{\n \"test\": 1,\n \"test2\": false\n}" -=- Just (JObject [("test", JNumber 1.0), ("test2", JFalse)])

test_pObject_nested :: Test.EasyCheck.Prop
test_pObject_nested = parse pObject "{\"test\": {\"hello\": \"world\"}}" -=- Just (JObject [("test", JObject [("hello", JString "world")])])

pArray :: Parser JValue
pArray =   JArray <$> (char '[' *> pWhitespace *> pArray' <* pWhitespace <* char ']')
       <|> JArray <$> (char '[' *> pWhitespace *> char ']' *> yield [])

pArray' :: Parser [JValue]
pArray' = (:) <$> (pWhitespace *> pJValue) <*> ((pWhitespace *> char ',' *> pArray') <|> yield [])

test_pArray_empty :: Test.EasyCheck.Prop
test_pArray_empty = parse pArray "[]" -=- Just (JArray [])

test_pArray_single :: Test.EasyCheck.Prop
test_pArray_single = parse pArray "[1]" -=- Just (JArray [JNumber 1.0])

test_pArray_multi :: Test.EasyCheck.Prop
test_pArray_multi = parse pArray "[true, false, null]" -=- Just (JArray [JTrue, JFalse, JNull])

test_pArray_nested :: Test.EasyCheck.Prop
test_pArray_nested = parse pArray "[true, [false], [[null]]]" -=- Just (JArray [JTrue, JArray [JFalse], JArray [JArray [JNull]]])

pWhitespace :: Parser ()
pWhitespace =   char ' ' *> pWhitespace
            <|> char '\n' *> pWhitespace
            <|> char '\r' *> pWhitespace
            <|> char '\t' *> pWhitespace
            <|> empty

pTrue :: Parser JValue
pTrue = word "true" *> yield JTrue

pFalse :: Parser JValue
pFalse = word "false" *> yield JFalse

pNull :: Parser JValue
pNull = word "null" *> yield JNull

pJString :: Parser JValue
pJString = JString <$> pString

pString :: Parser String
pString = char '"' *> pCharSequence <* char '"'

pCharSequence :: Parser String
pCharSequence =   (++) <$> (char '\\' *> pEscaped) <*> pCharSequence
              <|> (:) <$> check (\c -> c /= '"' && c /= '\\') anyChar <*> pCharSequence
              <|> yield ""

pEscaped :: Parser String
pEscaped =   char '"' *> yield "\""
         <|> char '\\' *> yield "\\"
         <|> char '/' *> yield "/"
         <|> char 'b' *> yield "\b"
         <|> char 'f' *> yield "\f"
         <|> char 'n' *> yield "\n"
         <|> char 'r' *> yield "\r"
         <|> char 't' *> yield "\t"
         <|> ((:[]) . chr) <$> (char 'u' *> pTwoByteHex)

pTwoByteHex :: Parser Int
pTwoByteHex = hexToInt <$> ((:) <$> pHexDigit <*> ((:) <$> pHexDigit <*> ((:) <$> pHexDigit <*> ((:[]) <$> pHexDigit))))
  where pHexDigit = check isHexDigit anyChar

hexToInt :: String -> Int
hexToInt s = foldl1 ((+).(16*)) (map digitToInt s)

test_pCharSequence_simple :: Test.EasyCheck.Prop
test_pCharSequence_simple = parse pCharSequence "test" -=- Just "test"

test_pCharSequence_noDoubleQuote :: Test.EasyCheck.Prop
test_pCharSequence_noDoubleQuote = parse pCharSequence "te\"st" -=- Nothing

test_pCharSequence_noStandaloneBackslash :: Test.EasyCheck.Prop
test_pCharSequence_noStandaloneBackslash = parse pCharSequence "He\\world" -=- Nothing

test_pCharSequence_escapedDoubleQuote :: Test.EasyCheck.Prop
test_pCharSequence_escapedDoubleQuote = parse pCharSequence "Hello \\\"World\\\"" -=- Just "Hello \"World\""

test_pCharSequence_escapedBackslash :: Test.EasyCheck.Prop
test_pCharSequence_escapedBackslash = parse pCharSequence "He\\\\world" -=- Just "He\\world"

test_pCharSequence_escapedSlash :: Test.EasyCheck.Prop
test_pCharSequence_escapedSlash = parse pCharSequence "He\\/world" -=- Just "He/world"

test_pCharSequence_escapedBackspace :: Test.EasyCheck.Prop
test_pCharSequence_escapedBackspace = parse pCharSequence "He\\bworld" -=- Just "He\bworld"

test_pCharSequence_escapedFormFeed :: Test.EasyCheck.Prop
test_pCharSequence_escapedFormFeed = parse pCharSequence "He\\fworld" -=- Just "He\fworld"

test_pCharSequence_escapedNewline :: Test.EasyCheck.Prop
test_pCharSequence_escapedNewline = parse pCharSequence "He\\nworld" -=- Just "He\nworld"

test_pCharSequence_escapedCarriageReturn :: Test.EasyCheck.Prop
test_pCharSequence_escapedCarriageReturn = parse pCharSequence "He\\rworld" -=- Just "He\rworld"

test_pCharSequence_escapedTab :: Test.EasyCheck.Prop
test_pCharSequence_escapedTab = parse pCharSequence "He\\tworld" -=- Just "He\tworld"

test_pCharSequence_twoEscapes :: Test.EasyCheck.Prop
test_pCharSequence_twoEscapes = parse pCharSequence "He\\r\\nWorld" -=- Just "He\r\nWorld"

test_pCharSequence_escapedUnicodeChar :: Test.EasyCheck.Prop
test_pCharSequence_escapedUnicodeChar = parse pCharSequence "Hello \\u2603 World" -=- Just "Hello ☃ World"

test_pCharSequence_escapedUnicodeRequiresFourDigits :: Test.EasyCheck.Prop
test_pCharSequence_escapedUnicodeRequiresFourDigits = parse pCharSequence "Hello \\u26 World" -=- Nothing

test_pString_simple :: Test.EasyCheck.Prop
test_pString_simple = parse pString "\"Hello, World\"" -=- Just "Hello, World"

test_pString_complex :: Test.EasyCheck.Prop
test_pString_complex = parse pString "\"Hello \\r\\n \\u2603 World\"" -=- Just "Hello \r\n ☃ World"

pJNumber :: Parser JValue
pJNumber = JNumber <$> pNumber

pNumber :: Parser Float
pNumber =   negateFloat <$> (char '-' *> pPositiveFloat)
        <|> pPositiveFloat

-- number without decimal point, decimal digits, base 10 exponent
toFloat :: Int -> Int -> Int -> Float
toFloat n d e = (fromInt n) * (10.0 ^ (d + e))

pPositiveFloat :: Parser Float
pPositiveFloat = (uncurry toFloat) <$> pWithDecimalPoint <*> pExponent

pExponent :: Parser Int
pExponent =   (char 'e' <|> char 'E') *> (char '-' *> yield negate <|> char '+' *> yield id <|> yield id) <*> pInt
          <!> yield 0

pWithDecimalPoint :: Parser (Int, Int)
pWithDecimalPoint = combine <$> some pDigit <*> (char '.' *> some pDigit <|> yield "")
  where
    s2i cs = foldl1 ((+).(10*)) (map (\c' -> ord c' - ord '0') cs)
    combine n d = (s2i (n ++ d), negate $ length d)

pInt :: Parser Int
pInt = (\cs -> foldl1 ((+).(10*)) (map (\c' -> ord c' - ord '0') cs)) <$> some pDigit

pDigit :: Parser Char
pDigit = check isDigit anyChar
