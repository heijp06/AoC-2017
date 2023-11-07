import Data.Foldable     (for_)
import Test.Hspec
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Lib (part1)

examples :: [(String, Int)]
examples = [ ("1122", 3)
           , ("1111", 4)
           , ("1234", 0)
           , ("91212129", 9)
           ]

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = describe "roll, score" $ for_ examples test
  where
    test (captcha, sum') = it description assertion
      where
        description = captcha ++ " -> " ++ show sum'
        assertion = result `shouldBe` sum'
        result = part1 captcha
