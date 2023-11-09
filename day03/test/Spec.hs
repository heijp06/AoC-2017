import Data.Foldable     (for_)
import Test.Hspec
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Lib (part1, part2, stress)

examples1 :: [(Int, Int)]
examples1 = [ (1, 0)
            , (12, 3)
            , (23, 2)
            , (1024, 31)
            ]


examples2 :: [(Int, Int)]
examples2 = [ (1, 1)
            , (2, 1)
            , (3, 2)
            , (4, 4)
            , (5, 5)
            , (23, 806)
            ]

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = do
   describe "part1" $ for_ examples1 (test part1)
   describe "stress" $ for_ examples2 (test stress)
    where
      test func (position, value) = it description assertion
        where
          description = show position ++ " -> " ++ show value
          assertion = position `shouldBe` result
          result = func position


