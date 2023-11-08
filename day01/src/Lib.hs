module Lib
    ( part1
    , part2
    ) where

import Data.Char(digitToInt)

part1 :: String -> Int
part1 = calculateSum 1

part2 :: String -> Int
part2 xs = calculateSum (length xs `div` 2) xs

calculateSum :: Int -> String -> Int
calculateSum n xs = sum $ [ digitToInt a | (a, b) <- zip xs (rotate n xs), a == b ]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs