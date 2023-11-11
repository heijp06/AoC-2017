module Lib
    ( next
    , part1
    , part2
    ) where

import Data.List (maximumBy)

import Data.Set (Set, empty, insert, member)

part1 :: [Int] -> Int
part1 xs = fst $ go xs empty 0

go :: [Int] -> Set [Int] -> Int -> (Int, [Int])
go ys seen n | member ys seen = (n, ys)
go ys seen n = go (next ys) (insert ys seen) (n + 1)

part2 :: [Int] -> Int
part2 xs = part1 . snd $ go xs empty 0

next :: [Int] -> [Int]
next xs = zipWith (+) without toAdd
    where
        (number, index) = maximumBy (\(n1, i1) (n2, i2) -> compare (n1, -i1) (n2, -i2)) $ zip xs [0..]
        len = length xs
        (d, m) = divMod number $ len
        toAdd = rotate (len - index - 1) $ replicate m (d + 1) ++ replicate (len - m) d
        rotate n ys = drop n ys ++ take n ys
        without = take index xs ++ [0] ++ drop (index + 1) xs