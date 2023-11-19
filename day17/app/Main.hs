module Main (main) where

import Lib

main :: IO ()
main = do
    x <- read . head . lines <$> readFile "..\\data\\day17.txt"
    putStr "Part 1: "
    print $ part1 x
    putStr "Part 2: "
    print $ part2 x
