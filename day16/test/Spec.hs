import Test.Hspec hiding (example)
import Test.Hspec.Core.Runner
import Lib

example :: [String]
example = [ "s1"
          , "x3/4"
          , "pe/b"
          ]

main :: IO ()
main = hspecWith defaultConfig { configFailFast = True } $ do
    describe "Part 1" $ do
        it "solve \"abcde\" \"s1\" = \"eabcd\"" $ solve "abcde" [head example] `shouldBe` "eabcd"
        it "solve \"abcde\" \"pe/b\" = \"aecdb\"" $ solve "abcde" [example !! 2] `shouldBe` "aecdb"
        it "solve \"abcde\" \"x3/4\" = \"abced\"" $ solve "abcde" [example !! 1] `shouldBe` "abced"
        it "solve \"abcde\" example = \"baedc\"" $ solve "abcde" example `shouldBe` "baedc"
