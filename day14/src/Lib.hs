module Lib
    ( part1
    , part2
    ) where

import Data.List (partition)
import qualified Data.Set as Set
import Knot (hash)

type Square = (Int, Int)
type Region = Set.Set Square

part1 :: String -> Int
part1 = length . used

part2 :: String -> Int
part2 = length . foldr addSquare [] . used

used :: String -> [Square]
used xs = [ s | (s, bit) <- squares, bit == '1' ]
    where
        hashes = [ hash $ xs ++ "-" ++ show x | x <- [0..127] :: [Int] ]
        grid = concatMap (concatMap toBin) hashes
        squares = zip ((,) <$> [0..127] <*> [0..127]) grid

addSquare :: Square -> [Region] -> [Region]
addSquare square regions = Set.unions (Set.singleton square : adjacent) : notAdjacent
    where
        (adjacent, notAdjacent) = partition isAdjacent regions
        isAdjacent region = any (`Set.member` region) [ add square x | x <- [(0, 1), (1, 0), (0, -1), (-1, 0)] ]
        add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

toBin :: Char -> String
toBin '0' = "0000"
toBin '1' = "0001"
toBin '2' = "0010"
toBin '3' = "0011"
toBin '4' = "0100"
toBin '5' = "0101"
toBin '6' = "0110"
toBin '7' = "0111"
toBin '8' = "1000"
toBin '9' = "1001"
toBin 'a' = "1010"
toBin 'b' = "1011"
toBin 'c' = "1100"
toBin 'd' = "1101"
toBin 'e' = "1110"
toBin 'f' = "1111"
toBin x = error $ "Illegal character " ++ show x