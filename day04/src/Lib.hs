module Lib
    ( part1
    , part2
    ) where

import Data.List (nub, sort)

part1 :: Eq a => [[a]] -> Int
part1 = solve id

part2 :: Ord a => [[[a]]] -> Int
part2 = solve sort

solve :: Eq a => (a -> a) -> [[a]] -> Int
solve f xss = sum [ 1 | xs <- xss, (nub . map f) xs == map f xs ]