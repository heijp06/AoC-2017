module Main (main) where

import Lib (part1, part2)

main :: IO ()
main = do
    input <- head . lines <$> readFile "..\\data\\day03.txt"
    print input
