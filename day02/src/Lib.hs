module Lib
    ( part1
    , part2
    ) where

part1 :: [String] -> Int
part1 xs = sum [ maximum ys - minimum ys | ys <- getRows xs ]

part2 :: [String] -> Int
part2 xs = sum $ map rowResult $ getRows xs
    where
        rowResult row = head [ x `div` y | x <- row, y <- row, x /= y, x `mod` y == 0 ]

getRows :: [String] -> [[Int]]
getRows xs = [ [ read word | word <- words x ] | x <- xs ]
