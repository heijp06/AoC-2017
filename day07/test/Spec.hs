import Test.Hspec (describe, hspec, it, shouldBe)
import Lib (part1, part2)

example :: [String]
example = [ "pbga (66)"
          , "xhth (57)"
          , "ebii (61)"
          , "havc (66)"
          , "ktlj (57)"
          , "fwft (72) -> ktlj, cntj, xhth"
          , "qoyq (66)"
          , "padx (45) -> pbga, havc, qoyq"
          , "tknk (41) -> ugml, padx, fwft"
          , "jptl (61)"
          , "ugml (68) -> gyxo, ebii, jptl"
          , "gyxo (61)"
          , "cntj (57)"
          ]

main :: IO ()
main = hspec $ do
    describe "Part 1" $ do
        it "part1 example == \"tknk\"" $ part1 example `shouldBe` "tknk"
