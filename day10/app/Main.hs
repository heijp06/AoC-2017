module Main (main) where

import Data.List.Split (splitOn)

import Lib

main :: IO ()
main = do
    xs <- map read . splitOn "," <$> readFile "..\\data\\day10.txt"
    putStr "Part 1: "
    print $ part1 xs
    putStr "Part 2: "
    print $ part2 xs
