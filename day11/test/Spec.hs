import Data.Foldable (for_)
import Test.Hspec
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Lib (part1, part2)

examples :: [([String], Int)]
examples = [ (["ne", "ne", "ne"], 3)
           , (["ne", "ne", "sw", "sw"], 0)
           , (["ne", "ne", "s", "s"], 2)
           , (["se", "sw", "se", "sw", "sw"], 3)
           , (["n", "n"], 2)
           , (["n", "ne"], 2)
           , (["n", "se"], 1)
           , (["n", "s"], 0)
           , (["n", "sw"], 1)
           , (["n", "nw"], 2)
           , (["ne", "ne"], 2)
           , (["ne", "se"], 2)
           , (["ne", "s"], 1)
           , (["ne", "sw"], 0)
           , (["ne", "nw"], 1)
           ]

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = do
    describe "part1: directions -> distance" $ for_ examples test
        where
            test (directions, distance) = it description assertion
                where
                    description = show directions ++ " -> " ++ show distance
                    assertion = result `shouldBe` distance
                    result = part1 directions

