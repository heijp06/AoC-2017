module Lib
    ( part1
    , part2
    ) where

import Knot (hash)

part1 :: String -> Int
part1 xs = sum $ concatMap (map count) hashes
    where
        hashes = [ hash $ xs ++ "-" ++ show x | x <- [0..127] :: [Int] ]

part2 :: String -> Int
part2 = undefined

count :: Char -> Int
count '0' = 0
count '1' = 1
count '2' = 1
count '3' = 2
count '4' = 1
count '5' = 2
count '6' = 2
count '7' = 3
count '8' = 1
count '9' = 2
count 'a' = 2
count 'b' = 3
count 'c' = 2
count 'd' = 3
count 'e' = 3
count 'f' = 4
count x = error $ "Illegal character " ++ show x