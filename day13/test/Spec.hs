import Test.Hspec hiding (example)

import Lib

example :: [String]
example = [ "0: 3"
          , "1: 2"
          , "4: 4"
          , "6: 4"
          ]

main :: IO ()
main = hspec $ do
    describe "Part 1" $ do
        it "part1 example == 24" $ part1 example `shouldBe` 24

    describe "Part 2" $ do
        it "part2 example == 10" $ part2 example `shouldBe` 10
