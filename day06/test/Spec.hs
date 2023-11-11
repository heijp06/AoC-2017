import Test.Hspec (describe, hspec, it, shouldBe)
import Lib (part1, part2)

example :: [Int]
example = [ 0, 2, 7, 0 ]

main :: IO ()
main = hspec $ do
    describe "Part 1" $ do
        it "part1 example" $ part1 example `shouldBe` 5
