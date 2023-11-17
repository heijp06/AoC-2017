module Lib
    ( part1
    , part2
    ) where

import Data.List.Split (splitOn)

part1 :: [String] -> Int
part1 xs = sum [ depth * range | x <- xs, (depth, range) <- [parseLine x], depth `mod` (2 * range - 2) == 0 ]

part2 :: [String] -> Int
part2 = undefined

parseLine :: String -> (Int, Int)
parseLine line = case splitOn ": " line of
                    [depth, range] -> (read depth, read range)
                    _ -> error $ "Unknown line: " ++ line