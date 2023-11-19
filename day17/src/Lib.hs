module Lib
    ( part1
    , part2
    ) where

part1 :: Int -> Int
part1 step = (xs ++ xs) !! (position + 1)
    where
        (xs, position) = foldl (add step) ([0], 0) [1..2017]

part2 :: Int -> Int
part2 = undefined

add :: Int -> ([Int], Int) -> Int -> ([Int], Int)
add step (xs, position) x = (prefix ++ [x] ++ suffix, newPosition + 1)
    where
        newPosition = (position + step) `mod` length xs
        (prefix, suffix) = splitAt (newPosition + 1) xs