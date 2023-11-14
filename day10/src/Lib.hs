module Lib
    ( part1
    , part2
    , reverseAt
    , reverseFirst
    , rotateLeft
    , rotateRight
    , solve
    ) where

part1 :: [Int] -> Int
part1 = solve 256

part2 :: [Int] -> Int
part2 = undefined

solve :: Int -> [Int] -> Int
solve n xs = head result * (head . tail) result
    where
        (result, _, _) = foldl tie ([0..n-1], 0, 0) xs
        tie (ys, pos, skip) x = (reverseAt pos x ys, (pos + x + skip) `mod` n, skip + 1)

rotateLeft :: Int -> [a] -> [a]
rotateLeft n xs = let n' = n `mod` length xs in drop n' xs ++ take n' xs

rotateRight :: Int -> [a] -> [a]
rotateRight = rotateLeft . negate

reverseFirst :: Int -> [a] -> [a]
reverseFirst n xs = (reverse . take n) xs ++ drop n xs

reverseAt :: Int -> Int -> [a] -> [a]
reverseAt index len = rotateRight index . reverseFirst len . rotateLeft index