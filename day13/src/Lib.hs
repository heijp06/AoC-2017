module Lib
    ( part1
    , part2
    ) where

import Data.List.Split (splitOn)

type Sieve = [[Bool]]

part1 :: [String] -> Int
part1 xs = sum [ d * r | (d, r) <- map parseLine xs, d `mod` (2 * r - 2) == 0 ]

part2 :: [String] -> Int
part2 xs = solve 0 sieve
    where
        sieve = [ layer d r | (d, r) <- map parseLine xs ]

parseLine :: String -> (Int, Int)
parseLine line = case splitOn ": " line of
                    [depth, range] -> (read depth, read range)
                    _ -> error $ "Unknown line: " ++ line

layer :: Int -> Int -> [Bool]
layer d r = cycle [ (x + d) `mod` m == 0 | x <- [0..m-1] ]
    where
        m = 2 * r - 2

solve :: Int -> Sieve -> Int
solve n sieve
    | not (any head sieve) = n
solve n sieve = solve (n + 1) (map tail sieve)