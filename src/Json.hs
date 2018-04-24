module Json
    ( JValue (JNull, JString, JInt, JReal, JTrue, JFalse, JArray, JObject)
     ,stringify
     ,prettify
     ,findArrayVal
     ,findJValue
     ,(##)
     ,(#)
     ,(!)
     ,getParsedJValue
     ,jsonValue
     ,jsonTest
     ,updateArray
     ,updateMaybeArray
     ,updateObject
     ,updateMaybeObject
    ) where

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
    deriving (Read, Show, Eq)

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
--}

-- Manipulate JSON values
-- 
-- 
findArrayVal :: Int -> JValue -> Maybe JValue
findArrayVal index (JArray ja) = if (index < (length ja)) then Just (ja !! index) else Nothing
findArrayVal _ _ = Nothing

findObjVal :: String -> Object -> Maybe JValue
findObjVal key [] = Nothing
findObjVal key ((k,v):vs) = if key == k then Just v else findObjVal key vs

findJValue :: String -> JValue -> Maybe JValue
findJValue key (JObject vs) = findObjVal key vs 
findJValue key _ = Nothing

(##) :: JValue -> String  -> Maybe JValue
j ## s = findJValue s j

(#) :: Maybe JValue -> String -> Maybe JValue
mj # s = mj >>= (## s)

(!) :: Maybe JValue -> Int -> Maybe JValue
mj ! i = mj >>= (findArrayVal i)

-- j.data.nested.errorCode2
-- findJValue "data" j >>= findJValue "nested" >>= findJValue "errorCode2"
-- j ## "data" >>= (## "nested") >>= (## "errorCode2")
-- j ## "data"#"nested"#"errorCode2"
-- j ## "people" >>= findArrayVal 2
-- j ## "people" ! 2

splitObjectAt :: String -> Object -> (Object, Object)
splitObjectAt _ [] = ([], [])
splitObjectAt key ((k,v):kvs) = if key == k
                                then ([], ((k,v):kvs))
                                else ((k, v):(fst x), snd x) where
                                    x = splitObjectAt key kvs

updateObject :: JValue -> String -> JValue -> JValue
updateObject _ _ (JObject [])   = JObject []
updateObject v key (JObject jo) = if (findObjVal key jo == Nothing)
                                  then JObject jo
                                  else let (l , (k,jv):r) = splitObjectAt key jo in
                                      JObject (l ++ (k, v):r)
updateObject _ _ jv             = jv

updateMaybeObject :: Maybe JValue -> String -> JValue -> JValue
updateMaybeObject mv key (JObject jo) = case mv of
                                            Nothing -> JObject jo
                                            Just v -> updateObject v key (JObject jo)

updateArray :: JValue -> Int -> JValue -> JValue
updateArray _ _ (JArray [])     = JArray []
updateArray v index (JArray ja) = if (index < (length ja)) 
                                  then let (l , jv:r) = splitAt index ja in
                                      JArray (l ++ v:r) 
                                  else JArray ja
updateArray _ _ jv              = jv

updateMaybeArray :: Maybe JValue -> Int -> JValue -> JValue
updateMaybeArray mv index (JObject jo) = case mv of
                                            Nothing -> JObject jo
                                            Just v -> updateArray v index (JObject jo)

-- updateArray (\_ -> JString "Will") 2 (JArray [JString "Paul",JString "Ann",JString "Joe",JObject [("errorCode2",JReal 9900000.0),("message2",JString "Error")]])
-- updateObject (\_ -> JString "Will") "id" (JObject [("type",JString "one"),("id",JString "Joe"),("data",JObject [("errorCode",JReal 9.99e7),("message",JString "Error")])])
-- updateObject (\_ -> JInt (-1)) "errorCode2" (j ## "data"#"nested")

testJson = "{\"name\":\"Paul\",\"age\":13,\"likes\":[\"JavaScript\",\"C\",\"Haskell\"],\"married\":\"true\"}"
-- testObj = getParsedJValue $ parse jsonValue  "x" testJson
testJValue = JObject [("name",JString "Paul"),("age",JInt 9),("likes",JArray [JString "JavaScript",JString "C",JString "Haskell"]),("married",JString "true")]
-- updateObject (JReal 11.5) "age" testJValue
-- fmap (updateArray (JString "C++") 1) (testJValue ## "likes")
-- updateObject JNull "likes" testJValue
-- updateMaybeObject (fmap (updateArray (JString "C++") 1) (testJValue ## "likes")) "likes" testJValue
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
jsonObject = JObject <$> (char' '{' *> (keyValue `sepBy` (char' ',')) <* char' '}')

jsonValue' :: Parser JValue
jsonValue' = jsonTrue <|> jsonFalse <|> jsonNull <|> jsonString <|> try jsonReal <|> jsonInt <|> jsonArray <|> jsonObject

jsonValue = ws *> jsonValue'


s = stringify j

jv = parseTest jsonValue s

getParsedJValue :: Either a JValue -> Maybe JValue
getParsedJValue (Right jv) = Just jv
getParsedJValue (Left _) = Nothing

-- Zipper
-- {"type":4611,"id":100,"data":{"errorCode":2,"errorMessage":"invalid cameraId"}}
x = JObject [("type", JInt 4611),("id", JInt 100), ("array", JArray [JTrue, JFalse, JReal 1.23]), ("data", JObject [("errorCode", JInt 2),("errorMessage", JString "invalid cameraId")])]
--type Zipper (JValue, [Breadcrumb])

--(("id", JInt 100), JObject [("type", JInt 4611),("id", JInt 100), ("array", JArray [JTrue, JFalse, JReal 1.23]), ("data", JObject [("errorCode", JInt 2),("errorMessage", JString "invalid cameraId")])])
--
--((1, JFalse), JArray [JTrue, JReal 1.23])




jsonTest :: IO ()
jsonTest = do
               s <- readFile "sample.json"
               let ej = parse jsonValue "xxx" s
                   mj = getParsedJValue ej
               case mj of
                   Nothing -> putStrLn "Nothing"
                   Just jv -> putStrLn $ prettify jv

--            let mj = j ## "data"#"nested"#"errorCode2"
--            case mj of
--                Just x -> putStrLn $ show x 
--                _      -> putStrLn "Nothing"
