module Main (main) where

import Lib (part1, part2)

main :: IO ()
main = do
    input <- readInput
    print $ part1 input
    print $ part2 input

readInput :: IO [String]
readInput = lines <$> readFile "..\\data\\day02.txt"
