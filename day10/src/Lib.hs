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
import Data.Bits (xor)
import Data.Char (ord)
import Data.List.Split (chunksOf)
import Text.Printf (printf)

part1 :: [Int] -> Int
part1 = fst . solve 256

part2 :: String -> String
part2 xs = result
    where
        bytes = concat . replicate 64 $ map ord xs ++ [17, 31, 73, 47, 23]
        shuffled = snd $ solve 256 bytes
        result = concatMap (printf "%02x" . foldr1 xor) $ chunksOf 16 shuffled

solve :: Int -> [Int] -> (Int, [Int])
solve n xs = (liftA2 (*) head (head . tail) result, result)
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