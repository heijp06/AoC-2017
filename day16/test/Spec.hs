import Test.Hspec hiding (example)
import Lib

example :: String
example = "s1,x3/4,pe/b"

main :: IO ()
main = hspec $ do
    describe "Part 1" $ do
        it "solve \"abcde\" example = \"baedc\"" $ solve "abcde" example `shouldBe` "baedc"
