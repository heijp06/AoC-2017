module Main (main) where

import Lib

main :: IO ()
main = do
    xs <- lines <$> readFile "..\\data\\day20.txt"
    putStr "Part 1: "
    print $ part1 xs
    putStr "Part 2: "
    print $ part2 xs
