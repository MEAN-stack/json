import Data.List
import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Control.Applicative
import Control.Monad

-- JSON library
{--
{
    "type" : "one",
    "id" : 1.23,
    "data" :
    {
        "errorCode" : 99.9e6,
        "message" : "Error"
    },
    "people":
    [
        "Paul",
        "Ann",
        "Joe"
    ],
    "status" :
    {
        "age" : 56,
        "married":true
    }
}
--}

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

type Object = [(String, JValue)]

j = JObject [("type", JString "one"), ("id", JReal 1.23), ("data", JObject [("errorCode", JReal 99.9e6), ("message", JString "Error"), ("nested", JObject [("errorCode2", JReal 99.9e6), ("message2", JString "Error")])]), ("people", JArray [JString "Paul", JString "Ann", JString "Joe", JObject [("errorCode2", JReal 99.9e6), ("message2", JString "Error")]]), ("status", JObject [("age", JInt 56), ("married", JTrue)])]

stringify :: JValue -> String
stringify JNull = "null"
stringify (JString s) = show s
stringify (JInt i) = show i
stringify (JReal r) = show r
stringify JTrue = "true"
stringify JFalse = "false"
stringify (JArray vs) = "[" ++ (intercalate "," $ map stringify vs) ++ "]"
stringify (JObject os) = "{" ++ (intercalate "," $ map (\(key, val) -> show key ++ ":" ++ stringify val) os) ++ "}"

prettify :: String -> JValue -> String
prettify spacer (JArray vs)  = "\n" ++ spacer ++ "[\n" ++ (intercalate ",\n" $ map (prettify sp) vs) ++ "\n" ++ spacer ++ "]"
    where sp = spacer ++ "    "

prettify spacer (JObject os) = "\n" ++ spacer ++ "{\n" ++ sp ++ (intercalate (",\n" ++ sp) $ map (\(key, val) -> show key ++ " : " ++ rend sp val) os) ++ "\n" ++ spacer ++ "}"
    where sp = spacer ++ "    "
          rend sp (JArray vs)  = prettify sp (JArray vs)
          rend sp (JObject os) = prettify sp (JObject os)
          rend sp x            = stringify x

prettify spacer x = spacer ++ stringify x

-- main = putStrLn $ prettify "" j
-- findJValue "status" j >>= findJValue "age" >>= getInt
-- do
--      status <- findJValue "status" j
--      age <- findJValue "age" status
--      i <- getInt age
--      return i
        

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


-- test JSON to parse
-- {"type":"one","id":1.23,"data":{"errorCode":9.99e7,"message":"Error","nested":{"errorCode2":9.99e7,"message2":"Error"}},"people":["Paul","Ann","Joe",{"errorCode2":9.99e7,"message2":"Error"}],"status":{"age":56,"married":true}}

                
jsonTrue' :: Parser JValue
jsonTrue' = (string "true") *> (pure JTrue)

jsonFalse :: Parser JValue
jsonFalse = (string "false") *> (pure JFalse)

jsonNull :: Parser JValue
jsonNull = (string "null") *> (pure JNull)

jstring :: Parser String
jstring = char '"' *> (many (noneOf ['"'])) <* char '"'

jsonString' :: Parser JValue
jsonString' = fmap JString jstring

jarray :: Parser [JValue]
jarray = char' '[' *> (jsonValue `sepBy` (char' ',')) <* char ']'

jsonArray :: Parser JValue
jsonArray = fmap JArray $ lexeme jarray

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
jsonValue' = jsonTrue <|> jsonFalse <|> jsonNull <|> jsonString <|> jsonArray <|> jsonObject

jsonValue = ws *> jsonValue'

ws :: Parser String
ws = many (oneOf " \t\r\n")

--lexeme :: Parser a -> Parser a
lexeme p = p <* ws

char' = lexeme . char

jsonTrue = lexeme jsonTrue'

jsonString = lexeme jsonString'
