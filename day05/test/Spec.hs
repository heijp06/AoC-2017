import Test.Hspec (describe, hspec, it, shouldBe)
import Lib (part1, part2)

example :: [Int]
example = [ 0, 3, 0, 1, -3 ]

main :: IO ()
main = hspec $ do
    describe "Part 1" $ do
        it "part1 example" $ part1 example `shouldBe` 5

    describe "Part 2" $ do
        it "part2 example" $ part2 example `shouldBe` 10
