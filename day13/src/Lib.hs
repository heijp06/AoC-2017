module Lib
    ( part1
    , part2
    ) where

import Data.List.Split (splitOn)

part1 :: [String] -> Int
part1 xs = sum [ d * r | (d, r) <- map parseLine xs, d `mod` (2 * r - 2) == 0 ]

part2 :: [String] -> Int
part2 = undefined

parseLine :: String -> (Int, Int)
parseLine line = case splitOn ": " line of
                    [depth, range] -> (read depth, read range)
                    _ -> error $ "Unknown line: " ++ line