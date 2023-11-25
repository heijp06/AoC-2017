import Test.Hspec hiding (example)

import qualified Data.Map as Map

import Lib
import Rules

example :: [String]
example = [ "../.# => ##./#../..."
          , ".#./..#/### => #..#/..../..../#..#"
          ]

exampleExtended :: [String]
exampleExtended = [ "../.# => ##./#../..."
                  , "##/#. => ##./#../..."
                  , "##/.. => ##./#../..."
                  , "../.. => ##./#../..."
                  , ".#./..#/### => #..#/..../..../#..#"
                  , "##./#../... => #..#/..../..../#..#"
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

    describe "enhance" $ do
        it "enhance start" $ enhance (rules2, rules3) start `shouldBe` "#..#........#..#"
        it "enhance #..." $ enhance (rules2, rules3) "#..." `shouldBe` "##.#....."
    
    describe "breakup 2 2" $ do
        it "breakup 2 2 ['a'..'p']" $ breakup 2 2 ['a'..'p'] `shouldBe` ["abef", "cdgh", "ijmn", "klop"]
    
    describe "join 2 3" $ do
        it "join 2 3" $ join 2 3 [ "abcdefghi"
                           , "jklmnopqr"
                           , "stuvwxyzA"
                           , "BCDEFGHIJ"
                           ] `shouldBe` "abcjkldefmnoghipqrstuBCDvwxEFGyzAHIJ"
    
    describe "indexes" $ do
        it "indexes 2x2" $ indexes 2 2 `shouldBe` [0, 1, 4, 5, 2, 3, 6, 7, 8, 9, 12, 13, 10, 11, 14, 15]
        it "indexes 3x2" $ indexes 3 2 `shouldBe` [ 0, 1, 4, 5, 8, 9, 2, 3, 6, 7, 10, 11
                                                  , 12, 13, 16, 17, 20, 21, 14, 15, 18, 19, 22, 23
                                                  , 24, 25, 28, 29, 32, 33, 26, 27, 30, 31, 34, 35
                                                  ]
        it "indexes 2x3" $ indexes 2 3 `shouldBe` [ 0, 1, 2, 9, 10, 11, 3, 4, 5, 12, 13, 14, 6, 7, 8, 15, 16, 17
                                                  , 18, 19, 20, 27, 28, 29, 21, 22, 23, 30, 31, 32, 24, 25, 26, 33, 34, 35
                                                  ]
    
    describe "indexesBreakup" $ do
        it "indexesBreakup 3x2" $ indexesBreakup 3 2 `shouldBe` [ 0, 1, 6, 7, 2, 3, 8, 9, 4, 5, 10, 11
                                                                , 12, 13, 18, 19, 14, 15, 20, 21, 16, 17, 22, 23
                                                                , 24, 25, 30, 31, 26, 27, 32, 33, 28, 29, 34, 35
                                                                ]
    
    describe "Part 1" $ do
        it "part1 exampleExtended" $ part1 exampleExtended `shouldBe` 108