import Test.Hspec

import Lib

main :: IO ()
main = hspec $ do
    describe "rotate left" $ do
        it "rotateLeft 2" $ rotateLeft 2 "abcde" `shouldBe` "cdeab"

    describe "rotate right" $ do
        it "rotateRight 2" $ rotateRight 2 "abcde" `shouldBe` "deabc"
