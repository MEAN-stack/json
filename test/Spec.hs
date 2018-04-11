import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
    describe "head" $ do
        it "returns the first element of a list" $ do
            head [2,3,4] `shouldBe` (2 :: Int)

        it "returns the first element of an *arbitrary* list" $
            property $ \x xs -> head (x:xs) == (x :: Int)
      
        it "throws an exception if used with an empty list" $ do
            evaluate (head []) `shouldThrow` anyException