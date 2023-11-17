module Lib
    ( part1
    , part2
    ) where

import Data.List.Split (splitOn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map ((!))

type Program = Int
type Pipes = Map.Map Program [Program]
type Programs = Set.Set Program

part1 :: [String] -> Int
part1 xs = solve Set.empty (Set.singleton 0) (parse xs)

part2 :: [String] -> Int
part2 = undefined 

parse :: [String] -> Pipes
parse = foldr addRow Map.empty
    where
        addRow row pipes =
            case splitOn " <-> " row of
                [prog, progs] -> Map.insert (read prog) (map read (splitOn "," progs)) pipes
                _ -> error $ "Invalid row: " ++ row

solve :: Programs -> Programs -> Pipes -> Int
solve reachable current pipes
    | Set.null current = Set.size reachable
    | otherwise = solve newReachable newCurrent pipes
        where
            newReachable = Set.union reachable current
            newCurrent = Set.fromList [ to | from <- Set.toList current, to <- pipes ! from, Set.notMember to reachable ]
