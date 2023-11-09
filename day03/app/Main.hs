module Main (main) where

import Lib (part1, part2)

main :: IO ()
main = do
    input <- read . head . lines <$> readFile "..\\data\\day03.txt"
    putStr "Part1: "
    print $ part1 input
    putStr "Part2: "
    print $ part2 input
