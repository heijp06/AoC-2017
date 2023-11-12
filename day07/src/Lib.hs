module Lib
    ( part1
    , part2
    ) where

part1 :: [String] -> String
part1 xs = head [ x | x <- bottoms (map words xs), x `notElem` tops (map words xs) ]

part2 :: [String] -> String
part2 = undefined

bottoms :: [[String]] -> [String]
bottoms = map head

tops :: [[String]] -> [String]
tops = map (filter (/=',')) . concatMap (drop 3)