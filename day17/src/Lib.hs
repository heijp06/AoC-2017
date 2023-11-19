module Lib
    ( part1
    , part2
    ) where

part1 :: Int -> Int
part1 step = (xs ++ xs) !! (position + 1)
    where
        (xs, position) = foldl (add1 step) ([0], 0) [1..2017]

part2 :: Int -> Int
part2 = undefined

add1 :: Int -> ([Int], Int) -> Int -> ([Int], Int)
add1 step (xs, position) x = (prefix ++ [x] ++ suffix, newPosition)
    where
        newPosition = 1 + (position + step) `mod` length xs
        (prefix, suffix) = splitAt newPosition xs