import Data.Foldable     (for_)
import Test.Hspec
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Lib (part1, part2)

examples1 :: [(String, Int)]
examples1 = [ ("1122", 3)
            , ("1111", 4)
            , ("1234", 0)
            , ("91212129", 9)
            ]


examples2 :: [(String, Int)]
examples2 = [ ("1212", 6)
            , ("1221", 0)
            , ("123425", 4)
            , ("123123", 12)
            , ("12131415", 4)
            ]

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = do
   describe "part1: captcha -> sum" $ for_ examples1 (test part1)
   describe "part2: captcha -> sum" $ for_ examples2 (test part2)
    where
      test part (captcha, sum') = it description assertion
        where
          description = captcha ++ " -> " ++ show sum'
          assertion = result `shouldBe` sum'
          result = part captcha
