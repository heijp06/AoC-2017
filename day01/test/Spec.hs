import Data.Foldable     (for_)
import Test.Hspec
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Lib (part1, part2)

examples :: [(String -> Int, String, Int)]
examples = [ (part1, "1122", 3)
           , (part1, "1111", 4)
           , (part1, "1234", 0)
           , (part1, "91212129", 9)
           , (part2, "1212", 6)
           , (part2, "1221", 0)
           , (part2, "123425", 4)
           , (part2, "123123", 12)
           , (part2, "12131415", 4)
           ]

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = describe "captcha -> sum" $ for_ examples test
  where
    test (part, captcha, sum') = it description assertion
      where
        description = captcha ++ " -> " ++ show sum'
        assertion = result `shouldBe` sum'
        result = part captcha
