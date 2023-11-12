module Main (main) where

import Lib (part1, part2)

main :: IO ()
main = do
    xs <- lines <$> readFile "..\\data\\day08.txt"
    putStr "Part 1: "
    print $ part1 xs
    putStr "Part 2: "
    print $ part2 xs
