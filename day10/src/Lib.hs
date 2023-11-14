module Lib
    ( part1
    , part2
    , reverseAt
    , reverseFirst
    , rotateLeft
    , rotateRight
    , solve
    ) where

import Control.Applicative (liftA2)

part1 :: [Int] -> Int
part1 = solve 256

part2 :: String -> String
part2 = undefined

solve :: Int -> [Int] -> Int
solve n xs = liftA2 (*) head (head . tail) result
    where
        (result, _, _) = foldl tie ([0..n-1], 0, 0) xs
        tie (ys, pos, skip) x = (reverseAt pos x ys, (pos + x + skip) `mod` n, skip + 1)

reverseAt :: Int -> Int -> [a] -> [a]
reverseAt index len = rotateRight index . reverseFirst len . rotateLeft index

rotateRight :: Int -> [a] -> [a]
rotateRight = rotateLeft . negate

rotateLeft :: Int -> [a] -> [a]
rotateLeft n xs = let n' = n `mod` length xs in drop n' xs ++ take n' xs

reverseFirst :: Int -> [a] -> [a]
reverseFirst n xs = (reverse . take n) xs ++ drop n xs