import Data.Foldable (for_)
import Test.Hspec
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)
import Lib

examples :: [(String, Int)]
examples = [ ("{}", 1)
           , ("{{{}}}", 6)
           , ("{{},{}}", 5)
           , ("{{{},{},{{}}}}", 16)
           , ("{<a>,<a>,<a>,<a>}", 1)
           , ("{{<ab>},{<ab>},{<ab>},{<ab>}}", 9)
           , ("{{<!!>},{<!!>},{<!!>},{<!!>}}", 9)
           , ("{{<a!>},{<a!>},{<a!>},{<ab>}}", 3)
           ]

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = do
   describe "Part 1" $ for_ examples test
    where
      test (stream, score) = it description assertion
        where
          description = stream ++ " -> " ++ show score
          assertion = result `shouldBe` score
          result = part1 stream
