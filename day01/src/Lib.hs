module Lib
    ( part1
    , part2
    ) where

import Data.Char(digitToInt)

part1 :: String -> Int
part1 xs = sum $ [ a | (a, b) <- zip digits (tail digits), a == b ]
    where
        digits = [ digitToInt c | c <- xs ++ [head xs] ]

part2 :: String -> Int
part2 xs = sum $ [ a | (a, b) <- zip digits cycled, a == b ]
    where
        digits = [ digitToInt c | c <- xs ]
        n = length xs `div` 2
        cycled = drop n digits ++ take n digits