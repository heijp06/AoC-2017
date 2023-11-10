module Lib
    ( part1
    , part2
    ) where

import Data.List (nub, sort)

part1 :: [[String]] -> Int
part1 xss = sum [ 1 | xs <- xss, nub xs == xs ]

part2 :: [[String]] -> Int
part2 xss = sum [ 1 | xs <- xss, (nub . map sort) xs == map sort xs ]