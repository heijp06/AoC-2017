module Main (main) where

import Lib (part1, part2)

main :: IO ()
main = do
    xs <- map read . words . head . lines <$> readFile "..\\data\\day06.txt"
    print xs
    putStr "Part 1: "
    print $ part1 xs
    putStr "Part 2: "
    print $ part2 xs
