module Main (main) where

import Lib (part1, part2)

main :: IO ()
main = do
    input <- map words . lines <$> readFile "..\\data\\day04.txt"
    putStr "Part 1: "
    print $ part1 input
    putStr "Part 2: "
    print $ part2 input