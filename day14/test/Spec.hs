import Test.Hspec hiding (example)
import Lib

example :: String
example = "flqrgnkx"

main :: IO ()
main = hspec $ do
    describe "Part 1" $ do
        it "part1 example == 8108" $ part1 example `shouldBe` 8108
