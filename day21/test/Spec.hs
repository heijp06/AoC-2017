import Test.Hspec hiding (example)

import qualified Data.Map as Map

import Lib
import Rules

example :: [String]
example = [ "../.# => ##./#../..."
          , ".#./..#/### => #..#/..../..../#..#"
          ]

rules2 :: Rules
rules2 = Map.fromList [ ("#...", "##.#.....")
                      , (".#..", "##.#.....")
                      , ("..#.", "##.#.....")
                      , ("...#", "##.#.....")
                      ]

rules3 :: Rules
rules3 = Map.fromList [ ("####...#.", "#..#........#..#")
                      , ("..##.#.##", "#..#........#..#")
                      , (".#...####", "#..#........#..#")
                      , ("##.#.##..", "#..#........#..#")
                      , ("###..#.#.", "#..#........#..#")
                      , ("#..#.###.", "#..#........#..#")
                      , (".#.#..###", "#..#........#..#")
                      , (".###.#..#", "#..#........#..#")
                      ]

main :: IO ()
main = hspec $ do
    describe "rotate2" $ do
        it "rotate2 #... -> .#.." $ rotate2 "#..." `shouldBe` ".#.."
        it "rotate2 .#.. -> ...#" $ rotate2 ".#.." `shouldBe` "...#"
        it "rotate2 ...# -> ..#." $ rotate2 "...#" `shouldBe` "..#."
        it "rotate2 ..#. -> #..." $ rotate2 "..#." `shouldBe` "#..."

    describe "rotate3" $ do
        it "rotate3 .#...#### -> #..#.###." $ rotate3 ".#...####" `shouldBe` "#..#.###."
        it "rotate3 #..#.###. -> ####...#." $ rotate3 "#..#.###." `shouldBe` "####...#."
        it "rotate3 ####...#. -> .###.#..#" $ rotate3 "####...#." `shouldBe` ".###.#..#"
        it "rotate3 .###.#..# -> .#...####" $ rotate3 ".###.#..#" `shouldBe` ".#...####"

    describe "flip3" $ do
        it "flip3 .#...#### -> .#.#..###" $ flip3 ".#...####" `shouldBe` ".#.#..###"
        it "flip3 #..#.###. -> ..##.#.##" $ flip3 "#..#.###." `shouldBe` "..##.#.##"
        it "flip3 ####...#. -> ###..#.#." $ flip3 "####...#." `shouldBe` "###..#.#."
        it "flip3 .###.#..# -> ##.#.##.." $ flip3 ".###.#..#" `shouldBe` "##.#.##.."
    
    describe "create" $ do
        it "create example[0]" $ create rotate2 flip2 (head example) `shouldBe` rules2
        it "create example[1]" $ create rotate3 flip3 (example !! 1) `shouldBe` rules3

    describe "parse" $ do
        it "parse example" $ parse example `shouldBe` (rules2, rules3)
