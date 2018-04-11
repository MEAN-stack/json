import Json
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Text.ParserCombinators.Parsec hiding ((<|>), many)

main :: IO ()
main = hspec $ do
    describe "stringify" $ do
        it "converts a JValue to a string" $ do
            (stringify $ JObject [("name", JString "Paul"), ("id", JReal 1.23), ("beer", JTrue)]) `shouldBe` "{\"name\":\"Paul\",\"id\":1.23,\"beer\":true}"

    describe "jsonValue" $ do
        it "parses a string to a JValue" $ do
            parse jsonValue "test" "{\"name\":\"Paul\",\"id\":1.23,\"beer\":true}" `shouldBe` Right (JObject [("name", JString "Paul"), ("id", JReal 1.23), ("beer", JTrue)])

    describe "object access" $ do
        it "accesses nested objects" $ do
            let jv = getParsedJValue $ parse jsonValue "test" "{\"name\":{\"first\":\"Paul\",\"last\":\"Robertson\"},\"likes\":[\"beer\",\"climbing\",\"computing\"]}"
            jv#"name"#"last" `shouldBe` Just (JString "Robertson")

    describe "array access" $ do
        it "accesses array objects" $ do
            let jv = getParsedJValue $ parse jsonValue "test" "{\"name\":{\"first\":\"Paul\",\"last\":\"Robertson\"},\"likes\":[\"beer\",\"climbing\",\"computing\"]}"
            jv # "likes" ! 1 `shouldBe` Just (JString "climbing")


--            head [2,3,4] `shouldBe` (2 :: Int)
--
--        it "returns the first element of an *arbitrary* list" $
--            property $ \x xs -> head (x:xs) == (x :: Int)
--      
--        it "throws an exception if used with an empty list" $ do
--            evaluate (head []) `shouldThrow` anyException

{--
JValue
     stringify
     prettify
     findArrayVal
     findJValue
     (##)
     (#)
     (!)
     getParsedJValue
     jsonValue
     jsonTest

j = JObject [("type", JString "one"), ("id", JReal 1.23), ("data", JObject [("errorCode", JReal 99.9e6), ("message", JString "Error"), ("nested", JObject [("errorCode2", JReal (-99.9e-6)), ("message2", JString "Error")])]), ("people", JArray [JString "Paul", JString "Ann", JString "Joe", JObject [("errorCode2", JReal 9.9E6), ("message2", JString "Error")]]), ("status", JObject [("age", JInt 56), ("married", JTrue)])]

stringify :: JValue -> String
prettify :: JValue -> String
findArrayVal :: Int -> JValue -> Maybe JValue
findJValue :: String -> JValue -> Maybe JValue
(##) :: JValue -> String  -> Maybe JValue
(#) :: Maybe JValue -> String -> Maybe JValue
(!) :: Maybe JValue -> Int -> Maybe JValue
jsonValue = ws *> jsonValue'
getParsedJValue :: Either a JValue -> Maybe JValue
jsonTest :: IO ()
--}

