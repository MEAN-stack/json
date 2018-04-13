import Json
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Text.ParserCombinators.Parsec hiding ((<|>), many)

main :: IO ()
main = hspec $ do
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


