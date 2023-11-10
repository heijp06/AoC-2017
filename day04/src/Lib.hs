module Lib
    ( part1
    , part2
    ) where

import Data.List (nub, sort)

part1 :: [[String]] -> Int
part1 = solve id

part2 :: [[String]] -> Int
part2 = solve sort

solve :: (String -> String) -> [[String]] -> Int
solve f xss = sum [ 1 | xs <- xss, (nub . map f) xs == map f xs ]