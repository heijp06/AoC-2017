module Lib
    ( part1
    , part2
    ) where

import Data.List.Split (splitOn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map ((!))

part1 :: [String] -> Int
part1 xs = solve Set.empty (Set.singleton 0) (parse xs)

part2 :: [String] -> Int
part2 = undefined 

parse :: [String] -> Map.Map Int [Int]
parse = foldr addRow Map.empty
    where
        addRow x m = let [prog, progs] = splitOn " <-> " x in Map.insert (read prog) (map read (splitOn "," progs)) m

solve :: Set.Set Int -> Set.Set Int -> Map.Map Int [Int] -> Int
solve reachable current pipes
    | Set.null current = Set.size reachable
    | otherwise = solve newReachable newCurrent pipes
        where
            newReachable = Set.union reachable current
            newCurrent = Set.fromList [ to | from <- Set.toList current, to <- pipes ! from, not $ Set.member to reachable ]
