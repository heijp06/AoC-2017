module Lib
    ( part1
    , part2
    , rotateLeft
    , rotateRight
    ) where

part1 :: [Int] -> Int
part1 = undefined

part2 :: [Int] -> Int
part2 = undefined

rotateLeft :: Int -> [a] -> [a]
rotateLeft n xs = let n' = n `mod` length xs in drop n' xs ++ take n' xs

rotateRight :: Int -> [a] -> [a]
rotateRight = rotateLeft . negate
