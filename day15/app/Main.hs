module Main (main) where

import Data.List.Split (splitOn)
import Lib

main :: IO ()
main = do
    xs <- map (read . last . splitOn " ") . lines <$> readFile "..\\data\\day15.txt"
    let a = head xs
    let b = head $ tail xs
    putStr "Part 1: "
    print $ part1 a b
    putStr "Part 2: "
    print $ part2 a b
