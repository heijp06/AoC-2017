import Test.Hspec hiding (example)

import Lib

example :: [String]
example = [ "set a 1"
          , "add a 2"
          , "mul a a"
          , "mod a 5"
          , "snd a"
          , "set a 0"
          , "rcv a"
          , "jgz a -1"
          , "set a 1"
          , "jgz a -2"
          ]

main :: IO ()
main = hspec $ do
    describe "Part 1" $ do
        it "part1 example == 4" $ part1 example `shouldBe` 4
