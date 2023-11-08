import Test.Hspec
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Lib (part1, part2)

example1 :: [String]
example1 = [ "5 1 9 5"
           , "7 5 3"
           , "2 4 6 8"
           ]

example2 :: [String]
example2 = [ "5 9 2 8"
           , "9 4 7 3"
           , "3 8 6 5"
           ]

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = do
    describe "part1" $
        it "Example part1 == 18" $ part1 example1 `shouldBe` 18

    describe "part2" $
        it "Example part2 == 9" $ part2 example2 `shouldBe` 9
