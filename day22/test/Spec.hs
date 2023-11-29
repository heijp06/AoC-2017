import Test.Hspec hiding (example)

import Lib

example :: [String]
example = [ "..#"
          , "#.."
          , "..."
          ]

main :: IO ()
main = hspec $ do
    describe "Part 1" $ do
        it "part1" $ part1 example `shouldBe` 5587

    describe "Part 2" $ do
        it "part2" $ part2 example `shouldBe` 2511944

