import Data.List
import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Control.Applicative
import Control.Monad

-- JSON library

-- example JavaScript object:
{--
{
    "type" : "one",
    "id" : 1.23,
    "data" :
    {
        "errorCode" : 99.9e6,
        "message" : "Error",
        "nested" :
        {
            "errorCode2" : -99.9e-6,
            message2 : "Error"
        }
    },
    "people" :
    [
        "Paul",
        "Ann",
        "Joe",
        {
            errorCode2 : 9.9E6,
            message2 : "Error"
        }
    ],
    "status" :
    {
        "age" : 56,
        "married" : true
    }
}
--}

-- Declare a Haskell data type for JSON value 
--
data JValue =
    JNull 
    | JString String
    | JInt Int
    | JReal Double
    | JTrue
    | JFalse
    | JArray [JValue]
    | JObject [(String, JValue)]
    deriving (Read, Show)

-- a JSON object is a lisk of key-value pairs
--
type Object = [(String, JValue)]

-- construct a sample JSON value
-- make it the same as the example JavaScript object above
--
j = JObject [("type", JString "one"), ("id", JReal 1.23), ("data", JObject [("errorCode", JReal 99.9e6), ("message", JString "Error"), ("nested", JObject [("errorCode2", JReal (-99.9e-6)), ("message2", JString "Error")])]), ("people", JArray [JString "Paul", JString "Ann", JString "Joe", JObject [("errorCode2", JReal 9.9E6), ("message2", JString "Error")]]), ("status", JObject [("age", JInt 56), ("married", JTrue)])]

-- convert a Haskell JValue to a string
--
-- stringify produces a compact single-line output
--
stringify :: JValue -> String
stringify JNull = "null"
stringify (JString s) = show s
stringify (JInt i) = show i
stringify (JReal r) = show r
stringify JTrue = "true"
stringify JFalse = "false"
stringify (JArray vs) = "[" ++ (intercalate "," $ map stringify vs) ++ "]"
stringify (JObject os) = "{" ++ (intercalate "," $ map (\(key, val) -> show key ++ ":" ++ stringify val) os) ++ "}"

-- prettify produces a multi-line, indented output
--
prettify' :: String -> JValue -> String
prettify' spacer (JArray vs)  = "\n" ++ spacer ++ "[\n" ++ (intercalate ",\n" $ map (prettify' sp) vs) ++ "\n" ++ spacer ++ "]"
    where sp = spacer ++ "    "

prettify' spacer (JObject os) = "\n" ++ spacer ++ "{\n" ++ sp ++ (intercalate (",\n" ++ sp) $ map (\(key, val) -> show key ++ " : " ++ rend sp val) os) ++ "\n" ++ spacer ++ "}"
    where sp = spacer ++ "    "
          rend sp (JArray vs)  = prettify' sp (JArray vs)
          rend sp (JObject os) = prettify' sp (JObject os)
          rend sp x            = stringify x

prettify' spacer x = spacer ++ stringify x

prettify :: JValue -> String
prettify = prettify' ""

{--
-- These helpers don't seem very helpful
--
isNull :: JValue -> Bool
isNull JNull = True
isNull _ = False

isString :: JValue -> Bool
isString (JString v) = True
isString _ = False

isInt :: JValue -> Bool
isInt (JInt v) = True
isInt _ = False

isReal :: JValue -> Bool
isReal (JReal v) = True
isReal _ = False

isTrue :: JValue -> Bool
isTrue JTrue = True
isTrue _ = False

isFalse :: JValue -> Bool
isFalse JFalse = True
isFalse _ = False

isArray :: JValue -> Bool
isArray (JArray v) = True
isArray _ = False

isObject :: JValue -> Bool
isObject (JObject v) = True
isObject _ = False
--}

-- Manipulate JSON values
-- 
-- 
getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _ = Nothing

getInt :: JValue -> Maybe Int
getInt (JInt i) = Just i
getInt _ = Nothing

getReal :: JValue -> Maybe Double
getReal (JReal d) = Just d
getReal _ = Nothing

getObject :: JValue -> Maybe Object
getObject (JObject o) = Just o
getObject _ = Nothing

findJValue :: String -> JValue -> Maybe JValue
findJValue key (JObject vs) = findObjVal key vs 
findJValue key _ = Nothing

findObjVal :: String -> Object -> Maybe JValue
findObjVal key [] = Nothing
findObjVal key ((k,v):vs) = if key == k then Just v else findObjVal key vs

-- TODO
-- stuff like this (JavaScript):
-- console.log(j.data.nested.message2)
-- j.people[2] = "Will"
-- j.people.push(100)
-- delete j.status.age

-- Parsing
--
-- Whitespace consumer
--
ws :: Parser String
ws = many (oneOf " \t\r\n")

lexeme :: Parser a -> Parser a
lexeme p = p <* ws

char' = lexeme . char

-- simple values
--
jsonTrue :: Parser JValue
jsonTrue = lexeme $ (string "true") *> (pure JTrue)

jsonFalse :: Parser JValue
jsonFalse = lexeme $ (string "false") *> (pure JFalse)

jsonNull :: Parser JValue
jsonNull = lexeme $ (string "null") *> (pure JNull)

-- strings
--
jstring :: Parser String
jstring = char '"' *> (many (noneOf ['"'])) <* char '"'

jsonString :: Parser JValue
jsonString = lexeme $ fmap JString jstring

-- signed integers
--
jsign :: Parser Char
jsign = (char '+') <|> (char '-') <|> (oneOf "0123456789")

jint :: Parser String
jint = many (oneOf "0123456789")

jsonInt' :: Parser Int
jsonInt' = do
                sign <- jsign
                num <- jint
                if ( sign == '-' ) then
                    return ((-1) * (read num :: Int)) 
                else 
                    if ( sign == '+' ) then
                        return (read num :: Int)
                    else
                        return (read (sign:num) :: Int)

jsonInt :: Parser JValue
jsonInt = fmap JInt jsonInt'

-- real numbers
--
jsonRealExp' :: Parser Double
jsonRealExp' = do
                ip <- jsonInt'
                char '.'
                dp <- jint
                oneOf "eE"
                exp <- jsonInt'
                return  ( (10 ** (fromIntegral exp) ) * (fromIntegral $ signum ip ) * ((fromIntegral $ abs ip) + read ("0." ++ dp) :: Double))

jsonReal' :: Parser Double
jsonReal' = do
                ip <- jsonInt'
                char '.'
                dp <- jint
                return  ( (fromIntegral $ signum ip ) * ((fromIntegral $ abs ip) + read ("0." ++ dp) :: Double))

jsonReal :: Parser JValue
jsonReal = fmap JReal (try jsonRealExp' <|> jsonReal')

-- arrays
--
jarray :: Parser [JValue]
jarray = char' '[' *> (jsonValue `sepBy` (char' ',')) <* char ']'

jsonArray :: Parser JValue
jsonArray = fmap JArray $ lexeme jarray

-- objects
--
keyValue :: Parser (String, JValue)
keyValue  = do
               key <- jstring
               ws
               char' ':'
               value <- jsonValue
               return (key, value)

jsonObject :: Parser JValue
jsonObject = JObject <$> (char' '{' *> (keyValue `sepBy` (char' ',')) <* char '}')

jsonValue' :: Parser JValue
jsonValue' = jsonTrue <|> jsonFalse <|> jsonNull <|> jsonString <|> try jsonReal <|> jsonInt <|> jsonArray <|> jsonObject

jsonValue = ws *> jsonValue'


s = stringify j

jv = parseTest jsonValue s