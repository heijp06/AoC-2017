module Lib
    ( part1
    , part2
    , solve
    ) where

import Data.List.Split (splitOn)
import Data.List (partition)

part1 :: [String] -> String
part1 = solve ['a'..'p']

part2 :: [String] -> String
part2 commands = solve (solve ['a'..'p'] namePermutations) positionPermutations
    where
        (partners, noPartners) = partition ((=='p') . head) commands
        namePermutations = concat $ replicate 4 partners
        positionPermutations = concat $ replicate 16 noPartners

solve :: String -> [String] -> String
solve = foldl execute

execute :: String -> String -> String
execute xs command = case command of
                        ('s':ns) -> spin (read ns) xs
                        ('x':ys) -> case splitOn "/" ys of
                                        [p1, p2] -> exchange (read p1) (read p2) xs
                                        _ -> invalid command
                        ['p', a, '/', b] -> partner a b xs
                        _ -> invalid command

spin :: Int -> String -> String
spin n xs = let l = length xs - n in drop l xs ++ take l xs

exchange :: Int -> Int -> String -> String
exchange i1 i2 xs = partner (xs !! i1) (xs !! i2) xs

partner :: Char -> Char -> String -> String
partner c1 c2 = foldr swap []
    where
        swap c ys = case c of
                        _ | c == c1 -> c2:ys
                        _ | c == c2 -> c1:ys
                        _ -> c:ys

invalid :: String -> a
invalid command = error $ "Invalid command: " ++ command
