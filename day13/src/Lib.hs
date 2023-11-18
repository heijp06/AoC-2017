module Lib
    ( part1
    , part2
    ) where

import Data.List.Split (splitOn)

part1 :: [String] -> Int
part1 xs = sum [ d * r | (d, r) <- map parseLine xs, caught r d ]

part2 :: [String] -> Int
part2 xs = solve 0 [ layer d r | (d, r) <- map parseLine xs ]

parseLine :: String -> (Int, Int)
parseLine line = case splitOn ": " line of
                    [depth, range] -> (read depth, read range)
                    _ -> error $ "Unknown line: " ++ line

caught :: Int -> (Int -> Bool)
caught r = (==0) . (`mod` (2 * r - 2))

layer :: Int -> Int -> [Bool]
layer d r = cycle [ caught r (x + d) | x <- [0..2*r-3] ]

solve :: Int -> [[Bool]] -> Int
solve n sieve | not (any head sieve) = n
solve n sieve = solve (n + 1) (map tail sieve)