{-# LANGUAGE OverloadedStrings #-}

-- | Hand-rolled JSON encoding and decoding.
-- Replaces aeson with a minimal implementation that covers
-- the exact set of features needed by this project.
module Json
  ( -- * Core types
    Value(..)
  , JsonNumber(..)
  , JsonObject
    -- * Encoding
  , encode
  , encodeLazy
    -- * Decoding
  , decodeStrict
    -- * Object construction
  , object
  , (.=)
    -- * Object access (KeyMap-like API)
  , objectLookup
  , objectMember
  , objectSize
  , objectEmpty
    -- * ToJSON class for (.=) polymorphism
  , ToJSON(..)
  ) where

import Data.Char (chr, digitToInt, intToDigit, isDigit, isHexDigit, isSpace, ord)
import Data.Int (Int64)
import Data.Word (Word16)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- | JSON value representation.
-- Numbers are stored as either exact integers or doubles.
data Value
  = Object !JsonObject
  | Array  ![Value]
  | String !T.Text
  | Number !JsonNumber
  | Bool   !Bool
  | Null
  deriving (Show)

-- | JSON number: either an exact integer or a floating-point value.
data JsonNumber
  = JInt  !Int64
  | JDbl  !Double
  deriving (Show)

instance Eq JsonNumber where
  JInt a  == JInt b  = a == b
  JDbl a  == JDbl b  = a == b
  JInt a  == JDbl b  = fromIntegral a == b
  JDbl a  == JInt b  = a == fromIntegral b

instance Ord JsonNumber where
  compare (JInt a)  (JInt b)  = compare a b
  compare (JDbl a)  (JDbl b)  = compare a b
  compare (JInt a)  (JDbl b)  = compare (fromIntegral a :: Double) b
  compare (JDbl a)  (JInt b)  = compare a (fromIntegral b :: Double)

-- | A JSON object preserving insertion order.
-- Stored as a list of (key, value) pairs.
type JsonObject = [(T.Text, Value)]

instance Eq Value where
  Object a  == Object b  = a == b
  Array a   == Array b   = a == b
  String a  == String b  = a == b
  Number a  == Number b  = a == b
  Bool a    == Bool b    = a == b
  Null      == Null      = True
  _         == _         = False

instance Ord Value where
  compare Null       Null       = EQ
  compare Null       _          = LT
  compare _          Null       = GT
  compare (Bool a)   (Bool b)   = compare a b
  compare (Bool _)   _          = LT
  compare _          (Bool _)   = GT
  compare (Number a) (Number b) = compare a b
  compare (Number _) _          = LT
  compare _          (Number _) = GT
  compare (String a) (String b) = compare a b
  compare (String _) _          = LT
  compare _          (String _) = GT
  compare (Array a)  (Array b)  = compare a b
  compare (Array _)  _          = LT
  compare _          (Array _)  = GT
  compare (Object a) (Object b) = compare a b

-- ----------------------------------------------------------------
-- Object helpers (KeyMap-like API)
-- ----------------------------------------------------------------

objectLookup :: T.Text -> JsonObject -> Maybe Value
objectLookup = lookup

objectMember :: T.Text -> JsonObject -> Bool
objectMember key = any (\(k, _) -> k == key)

objectSize :: JsonObject -> Int
objectSize = length

objectEmpty :: JsonObject
objectEmpty = []

-- | Build an Object from a list of key-value pairs.
object :: [(T.Text, Value)] -> Value
object = Object

-- ----------------------------------------------------------------
-- ToJSON class for (.=) polymorphism
-- ----------------------------------------------------------------

class ToJSON a where
  toJSON :: a -> Value

instance ToJSON Value where
  toJSON = id

instance ToJSON T.Text where
  toJSON = String

instance ToJSON Int where
  toJSON = Number . JInt . fromIntegral

instance ToJSON Int64 where
  toJSON = Number . JInt

instance ToJSON Word16 where
  toJSON = Number . JInt . fromIntegral

instance ToJSON Double where
  toJSON d
    -- If the double is an exact integer, store as JInt for cleaner output
    | not (isNaN d || isInfinite d) && d == fromIntegral rounded && abs d < 2 ^ (53 :: Int) =
        Number (JInt rounded)
    | otherwise = Number (JDbl d)
    where rounded = round d :: Int64

instance ToJSON Bool where
  toJSON = Bool

instance {-# OVERLAPPABLE #-} ToJSON a => ToJSON (Maybe a) where
  toJSON Nothing  = Null
  toJSON (Just x) = toJSON x

instance {-# OVERLAPPABLE #-} ToJSON a => ToJSON [a] where
  toJSON xs = Array (map toJSON xs)

instance {-# OVERLAPPING #-} ToJSON [Value] where
  toJSON = Array

instance (ToJSON v) => ToJSON (Map.Map String v) where
  toJSON m = Object [(T.pack k, toJSON v) | (k, v) <- Map.toAscList m]

instance (ToJSON v) => ToJSON (Map.Map T.Text v) where
  toJSON m = Object [(k, toJSON v) | (k, v) <- Map.toAscList m]

-- | Build a key-value pair for use with 'object'.
(.=) :: ToJSON a => T.Text -> a -> (T.Text, Value)
(.=) key val = (key, toJSON val)
infixr 8 .=

-- ----------------------------------------------------------------
-- JSON Encoding
-- ----------------------------------------------------------------

-- | Encode a JSON Value to a strict ByteString.
encode :: Value -> BS.ByteString
encode = TE.encodeUtf8 . encodeText

-- | Encode a JSON Value to a lazy ByteString.
encodeLazy :: Value -> LBS.ByteString
encodeLazy = LBS.fromStrict . encode

-- | Encode a JSON Value to Text.
encodeText :: Value -> T.Text
encodeText v = T.pack (encodeValue v "")

-- | Encode to a ShowS-style difference list for efficient concatenation.
encodeValue :: Value -> ShowS
encodeValue Null         = showString "null"
encodeValue (Bool True)  = showString "true"
encodeValue (Bool False) = showString "false"
encodeValue (Number n)   = encodeNumber n
encodeValue (String s)   = encodeString s
encodeValue (Array xs)   = showChar '[' . encodeList xs . showChar ']'
encodeValue (Object kvs) = showChar '{' . encodeObj kvs . showChar '}'

encodeNumber :: JsonNumber -> ShowS
encodeNumber (JInt i) = shows i
encodeNumber (JDbl d)
  | isNaN d || isInfinite d = showString "null"
  | otherwise = showString (show d)

encodeString :: T.Text -> ShowS
encodeString s = showChar '"' . T.foldr (\c acc -> escapeChar c . acc) id s . showChar '"'

escapeChar :: Char -> ShowS
escapeChar '"'  = showString "\\\""
escapeChar '\\' = showString "\\\\"
escapeChar '\n' = showString "\\n"
escapeChar '\r' = showString "\\r"
escapeChar '\t' = showString "\\t"
escapeChar '\b' = showString "\\b"
escapeChar '\f' = showString "\\f"
escapeChar c
  | ord c < 0x20 = showString "\\u" . showHex4 (ord c)
  | otherwise = showChar c

showHex4 :: Int -> ShowS
showHex4 n =
  let d3 = n `div` 4096
      d2 = (n `mod` 4096) `div` 256
      d1 = (n `mod` 256) `div` 16
      d0 = n `mod` 16
  in showChar (intToDigit d3) . showChar (intToDigit d2)
   . showChar (intToDigit d1) . showChar (intToDigit d0)

encodeList :: [Value] -> ShowS
encodeList []     = id
encodeList [x]    = encodeValue x
encodeList (x:xs) = encodeValue x . showChar ',' . encodeList xs

encodeObj :: [(T.Text, Value)] -> ShowS
encodeObj []          = id
encodeObj [(k, v)]    = encodeString k . showChar ':' . encodeValue v
encodeObj ((k, v):rest) =
  encodeString k . showChar ':' . encodeValue v . showChar ',' . encodeObj rest

-- ----------------------------------------------------------------
-- JSON Decoding
-- ----------------------------------------------------------------

-- | Decode a strict ByteString into a JSON Value.
decodeStrict :: BS.ByteString -> Either String Value
decodeStrict bs =
  let t = TE.decodeUtf8 bs
      s = T.unpack t
  in case parseValue (skipWS s) of
       Left err -> Left err
       Right (v, rest)
         | all isSpace rest -> Right v
         | otherwise -> Left $ "trailing content: " ++ take 20 rest

-- | Parse state is just a String (remaining input).
type Parser a = String -> Either String (a, String)

parseValue :: Parser Value
parseValue [] = Left "unexpected end of input"
parseValue ('"':rest) = parseString rest >>= \(s, r) -> Right (String (T.pack s), r)
parseValue ('{':rest) = parseObject (skipWS rest)
parseValue ('[':rest) = parseArray (skipWS rest)
parseValue ('t':rest) = expectLit "rue" (Bool True) rest
parseValue ('f':rest) = expectLit "alse" (Bool False) rest
parseValue ('n':rest) = expectLit "ull" Null rest
parseValue s@(c:_)
  | c == '-' || isDigit c = parseNumber s
  | otherwise = Left $ "unexpected character: " ++ [c]

parseObject :: Parser Value
parseObject ('}':rest) = Right (Object [], rest)
parseObject s = parseObjPairs s [] >>= \(pairs, r) -> Right (Object (reverse pairs), r)

parseObjPairs :: String -> [(T.Text, Value)] -> Either String ([(T.Text, Value)], String)
parseObjPairs [] _ = Left "unexpected end of input in object"
parseObjPairs ('"':rest) acc = do
  (key, r1) <- parseString rest
  r2 <- expectChar ':' (skipWS r1)
  (val, r3) <- parseValue (skipWS r2)
  let acc' = (T.pack key, val) : acc
  case skipWS r3 of
    (',':r4) -> parseObjPairs (skipWS r4) acc'
    ('}':r4) -> Right (acc', r4)
    _        -> Left "expected ',' or '}' in object"
parseObjPairs (c:_) _ = Left $ "expected string key in object, got: " ++ [c]

parseArray :: Parser Value
parseArray (']':rest) = Right (Array [], rest)
parseArray s = parseArrItems s [] >>= \(items, r) -> Right (Array (reverse items), r)

parseArrItems :: String -> [Value] -> Either String ([Value], String)
parseArrItems s acc = do
  (val, r1) <- parseValue (skipWS s)
  let acc' = val : acc
  case skipWS r1 of
    (',':r2) -> parseArrItems (skipWS r2) acc'
    (']':r2) -> Right (acc', r2)
    _        -> Left "expected ',' or ']' in array"

parseString :: String -> Either String (String, String)
parseString = go []
  where
    go acc ('"':rest) = Right (reverse acc, rest)
    go acc ('\\':rest) = parseEscape rest >>= \(c, r) -> go (c : acc) r
    go _ [] = Left "unterminated string"
    go acc (c:rest) = go (c : acc) rest

parseEscape :: String -> Either String (Char, String)
parseEscape ('"':rest)  = Right ('"', rest)
parseEscape ('\\':rest) = Right ('\\', rest)
parseEscape ('/':rest)  = Right ('/', rest)
parseEscape ('n':rest)  = Right ('\n', rest)
parseEscape ('r':rest)  = Right ('\r', rest)
parseEscape ('t':rest)  = Right ('\t', rest)
parseEscape ('b':rest)  = Right ('\b', rest)
parseEscape ('f':rest)  = Right ('\f', rest)
parseEscape ('u':a:b:c:d:rest)
  | all isHexDigit [a, b, c, d] =
      let cp = digitToInt a * 4096 + digitToInt b * 256 + digitToInt c * 16 + digitToInt d
      in if cp >= 0xD800 && cp <= 0xDBFF
           then parseSurrogatePair cp rest
           else Right (chr cp, rest)
  | otherwise = Left "invalid unicode escape"
parseEscape _ = Left "invalid escape sequence"

-- | Parse the low surrogate of a surrogate pair.
parseSurrogatePair :: Int -> String -> Either String (Char, String)
parseSurrogatePair high ('\\':'u':a:b:c:d:rest)
  | all isHexDigit [a, b, c, d] =
      let low = digitToInt a * 4096 + digitToInt b * 256 + digitToInt c * 16 + digitToInt d
      in if low >= 0xDC00 && low <= 0xDFFF
           then let cp = 0x10000 + (high - 0xD800) * 0x400 + (low - 0xDC00)
                in Right (chr cp, rest)
           else Left "invalid low surrogate in surrogate pair"
  | otherwise = Left "invalid unicode escape in surrogate pair"
parseSurrogatePair _ _ = Left "expected low surrogate after high surrogate"

parseNumber :: Parser Value
parseNumber s =
  let (numStr, rest) = spanNumber s
  in if null numStr
       then Left "expected number"
       else if '.' `elem` numStr || 'e' `elem` numStr || 'E' `elem` numStr
              then case reads numStr :: [(Double, String)] of
                     [(d, "")] -> Right (Number (JDbl d), rest)
                     _         -> Left $ "invalid number: " ++ numStr
              else case reads numStr :: [(Int64, String)] of
                     [(i, "")] -> Right (Number (JInt i), rest)
                     _ -> case reads numStr :: [(Integer, String)] of
                            [(n, "")] ->
                              if n >= fromIntegral (minBound :: Int64)
                                 && n <= fromIntegral (maxBound :: Int64)
                                then Right (Number (JInt (fromIntegral n)), rest)
                                else Right (Number (JDbl (fromIntegral n)), rest)
                            _ -> Left $ "invalid number: " ++ numStr

spanNumber :: String -> (String, String)
spanNumber s =
  let (neg, s1) = case s of
                    ('-':r) -> ("-", r)
                    _       -> ("", s)
      (int, s2) = span isDigit s1
      (frac, s3) = case s2 of
                     ('.':r) -> let (ds, r') = span isDigit r
                                in ('.' : ds, r')
                     _       -> ("", s2)
      (ex, s4) = case s3 of
                   (e:r) | e == 'e' || e == 'E' ->
                     case r of
                       ('+':r') -> let (ds, r'') = span isDigit r'
                                   in (e : '+' : ds, r'')
                       ('-':r') -> let (ds, r'') = span isDigit r'
                                   in (e : '-' : ds, r'')
                       _        -> let (ds, r') = span isDigit r
                                   in (e : ds, r')
                   _ -> ("", s3)
  in (neg ++ int ++ frac ++ ex, s4)

expectLit :: String -> Value -> Parser Value
expectLit [] val rest = Right (val, rest)
expectLit _ _ [] = Left "unexpected end of input in literal"
expectLit (e:es) val (c:cs)
  | c == e    = expectLit es val cs
  | otherwise = Left $ "expected '" ++ [e] ++ "', got '" ++ [c] ++ "'"

expectChar :: Char -> String -> Either String String
expectChar expected (c:rest)
  | c == expected = Right rest
  | otherwise = Left $ "expected '" ++ [expected] ++ "', got '" ++ [c] ++ "'"
expectChar expected [] = Left $ "expected '" ++ [expected] ++ "', got end of input"

skipWS :: String -> String
skipWS = dropWhile isSpace
