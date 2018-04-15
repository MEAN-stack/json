import Json
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Text.ParserCombinators.Parsec hiding ((<|>), many)

main :: IO ()
main = hspec $ do
    describe "constructors" $ do
        it "constructs a null value" $ do
            show JNull `shouldBe` "JNull"

        it "constructs a true value" $ do
            show JTrue `shouldBe` "JTrue"

        it "constructs a False value" $ do
            show JFalse `shouldBe` "JFalse"

        it "constructs an integer value" $ do
            show (JInt (-1)) `shouldBe` "JInt (-1)"

        it "constructs a real value" $ do
            show (JReal 1.23) `shouldBe` "JReal 1.23"

        it "constructs a string value" $ do
            show (JString "hello") `shouldBe` "JString \"hello\""
                                                                                                                                                        
        it "constructs an array value" $ do
            show (JArray [JNull, JTrue, JString "hello"]) `shouldBe` "JArray [JNull,JTrue,JString \"hello\"]"

        it "constructs an object value" $ do
            show (JObject [("name", JString "Paul"), ("age", JInt 9), ("data", JNull)]) `shouldBe` "JObject [(\"name\",JString \"Paul\"),(\"age\",JInt 9),(\"data\",JNull)]"
                                                                                                                                                                        
    describe "stringify" $ do
        it "converts a JValue to a JSON string" $ do
            (stringify $ JObject [("name", JString "Paul"), ("id", JReal 1.23), ("beer", JTrue)]) `shouldBe` "{\"name\":\"Paul\",\"id\":1.23,\"beer\":true}"

    describe "jsonValue" $ do
        it "parses a JSON string to a JValue" $ do
            parse jsonValue "test" "{\"name\":\"Paul\",\"id\":1.23,\"beer\":true}" `shouldBe` Right (JObject [("name", JString "Paul"), ("id", JReal 1.23), ("beer", JTrue)])

        it "fails to parse invalid JSON" $ do
            let jv = getParsedJValue $ parse jsonValue "test" "{\"name\":Paul,\"id\":1.23,\"beer\":true}"
            jv `shouldBe` Nothing
    
    describe "object access" $ do
        it "accesses nested objects" $ do
            let jv = getParsedJValue $ parse jsonValue "test" "{\"name\":{\"first\":\"Paul\",\"last\":\"Robertson\"},\"likes\":[\"beer\",\"climbing\",\"computing\"]}"
            jv#"name"#"last" `shouldBe` Just (JString "Robertson")

        it "returns Nothing for an invalid key" $ do
            let jv = getParsedJValue $ parse jsonValue "test" "{\"name\":{\"first\":\"Paul\",\"last\":\"Robertson\"},\"likes\":[\"beer\",\"climbing\",\"computing\"]}"
            jv#"nam"#"last" `shouldBe` Nothing
            jv#"name"#"middle" `shouldBe` Nothing

    describe "array access" $ do
        it "accesses array objects" $ do
            let jv = getParsedJValue $ parse jsonValue "test" "{\"name\":{\"first\":\"Paul\",\"last\":\"Robertson\"},\"likes\":[\"beer\",\"climbing\",\"computing\"]}"
            jv # "likes" ! 1 `shouldBe` Just (JString "climbing")

        it "returns Nothing for an invalid index" $ do
            let jv = getParsedJValue $ parse jsonValue "test" "{\"name\":{\"first\":\"Paul\",\"last\":\"Robertson\"},\"likes\":[\"beer\",\"climbing\",\"computing\"]}"
            jv # "likes" ! 3 `shouldBe` Nothing

    describe "object modification" $ do
        it "modifies an object field identified by key" $ do
            let jo = updateObject (JString "Will") "id" (JObject [("type",JString "one"),("id",JString "Joe"),("data",JObject [("errorCode",JReal 9.99e7),("message",JString "Error")])])
            jo `shouldBe` (JObject [("type",JString "one"),("id",JString "Will"),("data",JObject [("errorCode",JReal 9.99e7),("message",JString "Error")])])

    describe "array modification" $ do
        it "modifies an array field at a specific index" $ do
            let ja = updateArray (JString "Will") 2 (JArray [JString "Paul",JString "Ann",JString "Joe",JObject [("errorCode2",JReal 9900000.0),("message2",JString "Error")]])
            ja `shouldBe` (JArray [JString "Paul",JString "Ann",JString "Will",JObject [("errorCode2",JReal 9900000.0),("message2",JString "Error")]])

        it "modifies an array within an object" $ do
            let jv = JObject [("name",JString "Paul"),("age",JInt 9),("likes",JArray [JString "JavaScript",JString "C",JString "Haskell"]),("married",JString "true")]
            let modv = updateMaybeObject (fmap (updateArray (JString "C++") 1) (jv ## "likes")) "likes" jv
            modv `shouldBe` (JObject [("name",JString "Paul"),("age",JInt 9),("likes",JArray [JString "JavaScript",JString "C++",JString "Haskell"]),("married",JString "true")])

        it "modifies an object within an object" $ do
            let jv = JObject [("name", JString "Paul"), ("age", JInt 9), ("likes", JObject [("food", JString "cheese"), ("language", JString "JavaScript")]), ("married", JString "true")]
            let modv = updateMaybeObject (fmap (updateObject (JString "Haskell") "language") (jv ## "likes")) "likes" jv
            modv `shouldBe` (JObject [("name", JString "Paul"), ("age", JInt 9), ("likes", JObject [("food", JString "cheese"), ("language", JString "Haskell")]), ("married", JString "true")])
    
    
        