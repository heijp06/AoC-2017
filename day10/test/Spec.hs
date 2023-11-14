import Data.Foldable (for_)
import Test.Hspec hiding (example)

import Lib

example :: [Int]
example = [3, 4, 1, 5]

examples2 :: [(String, String)]
examples2 = [ ("", "a2582a3a0e66e6e86e3812dcb672a272")
            , ("AoC 2017", "33efeb34ea91902bb2f59c9920caa6cd")
            , ("1,2,3", "3efbe78a8d82f29979031a4aa0b16a9d")
            , ("1,2,4", "63960835bcdc130f0b66d7ff4f6a5a8e")
            ]

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
    
    describe "part2: input -> hash" $ for_ examples2 test
    where
        test (input, hash) = it description assertion
            where
                description = input ++ " -> " ++ hash
                assertion = result `shouldBe` hash
                result = part2 input