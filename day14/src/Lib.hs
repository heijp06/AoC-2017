module Lib
    ( part1
    , part2
    ) where

import qualified Data.Set as Set
import Knot (hash)

type Square = (Int, Int)
type Region = Set.Set Square

part1 :: String -> Int
part1 xs = length . filter (=='1') $ concatMap (concatMap toBin) hashes
    where
        hashes = [ hash $ xs ++ "-" ++ show x | x <- [0..127] :: [Int] ]

part2 :: String -> Int
part2 xs = error $ show (length used)
    where
        hashes = [ hash $ xs ++ "-" ++ show x | x <- [0..127] :: [Int] ]
        grid = concatMap (concatMap toBin) hashes
        squares = zip ((,) <$> [0..127] <*> [0..127]) grid
        used = [ s | (s, bit) <- squares, bit == '1' ]

asd = undefined

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