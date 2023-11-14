module Main (main) where

import Data.List.Split (splitOn)

import Lib

main :: IO ()
main = do
    xs <- head . lines <$> readFile "..\\data\\day10.txt"
    let ys = map read $ splitOn "," xs
    putStr "Part 1: "
    print $ part1 ys
    putStr "Part 2: "
    putStrLn $ part2 xs
