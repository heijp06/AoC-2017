import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
    describe "Part 1" $ do
        it "part1 65 8921" $ part1 65 8921 `shouldBe` 588

