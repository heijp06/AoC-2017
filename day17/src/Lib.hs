module Lib
    ( part1
    , part2
    ) where

part1 :: Int -> Int
part1 step = (xs ++ xs) !! (position + 1)
    where
        (xs, position) = foldl (add1 step) ([0], 0) [1..2017]

part2 :: Int -> Int
part2 step = fst $ foldl (add2 step) (0, 0) [1..50000000]

add1 :: Int -> ([Int], Int) -> Int -> ([Int], Int)
add1 step (xs, position) x = (prefix ++ [x] ++ suffix, newPosition)
    where
        newPosition = 1 + (position + step) `mod` x
        (prefix, suffix) = splitAt newPosition xs

add2 :: Int -> (Int, Int) -> Int -> (Int, Int)
add2 step (value, position) x = (newValue, newPosition)
    where
        newPosition = 1 + (position + step) `mod` x
        newValue = if newPosition == 1 then x else value
