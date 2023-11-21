import Test.Hspec hiding (example)

import Lib

example :: [String]
example = [ "     |          "
          , "     |  +--+    "
          , "     A  |  C    "
          , " F---|----E|--+ "
          , "     |  |  |  D "
          , "     +B-+  +--+ "
          ]

main :: IO ()
main = hspec $ do
    describe "Part 1" $ do
        it "part1 example == ABCDEF" $ part1 example `shouldBe` "ABCDEF"
