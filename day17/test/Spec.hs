import Test.Hspec

import Lib

main :: IO ()
main = hspec $ do
    describe "Part 1" $ do
        it "part1 3 == 638" $ part1 3 `shouldBe` 638
