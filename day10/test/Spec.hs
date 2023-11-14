import Test.Hspec hiding (example)

import Lib

example :: [Int]
example = [3, 4, 1, 5]

main :: IO ()
main = hspec $ do
    describe "rotate left" $ do
        it "rotateLeft 2" $ rotateLeft 2 "abcde" `shouldBe` "cdeab"

    describe "rotate right" $ do
        it "rotateRight 2" $ rotateRight 2 "abcde" `shouldBe` "deabc"

    describe "reverseFirst" $ do
        it "reverseFirst 2" $ reverseFirst 2 "abcde" `shouldBe` "bacde"
    
    describe "reverseAt" $ do
        it "reverseAt 2 2" $ reverseAt 2 2 "abcde" `shouldBe` "abdce"
    
    describe "solve" $ do
        it "solve example" $ solve 5 example `shouldBe` 12