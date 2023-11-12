import Test.Hspec hiding (example)
import Lib (part1, part2)

example :: [String]
example = [ "b inc 5 if a > 1"
          , "a inc 1 if b < 5"
          , "c dec -10 if a >= 1"
          , "c inc -20 if c == 10"
          ]

main :: IO ()
main = hspec $ do
    describe "Part 1" $ do
        it "part1 example == 1" $ part1 example `shouldBe` 1
