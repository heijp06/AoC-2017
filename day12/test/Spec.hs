import Test.Hspec hiding (example)

import Lib

example :: [String]
example = [ "0 <-> 2"
          , "1 <-> 1"
          , "2 <-> 0, 3, 4"
          , "3 <-> 2, 4"
          , "4 <-> 2, 3, 6"
          , "5 <-> 6"
          , "6 <-> 4, 5"
          ]

main :: IO ()
main = hspec $ do
    describe "Part 1" $ do
        it "part1 example == 6" $ part1 example `shouldBe` 6

    describe "Part 2" $ do
        it "part2 example == 2" $ part2 example `shouldBe` 2
