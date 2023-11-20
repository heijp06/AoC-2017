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

example2 :: [String]
example2 = [ "snd 1"
           , "snd 2"
           , "snd p"
           , "rcv a"
           , "rcv b"
           , "rcv c"
           , "rcv d"
           ]

main :: IO ()
main = hspec $ do
    describe "Part 1" $ do
        it "part1 example == 4" $ part1 example `shouldBe` 4

    describe "Part 2" $ do
        it "part2 example == 3" $ part2 example `shouldBe` 3
