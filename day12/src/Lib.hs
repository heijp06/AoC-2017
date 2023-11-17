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
part1 xs = Set.size $ solve Set.empty (Set.singleton 0) (parse xs)

part2 :: [String] -> Int
part2 xs = solve2 [] (parse xs)

parse :: [String] -> Pipes
parse = foldr addRow Map.empty
    where
        addRow row pipes =
            case splitOn " <-> " row of
                [prog, progs] -> Map.insert (read prog) (map read (splitOn "," progs)) pipes
                _ -> error $ "Invalid row: " ++ row

solve :: Programs -> Programs -> Pipes -> Programs
solve reachable current pipes
    | Set.null current = reachable
    | otherwise = solve newReachable newCurrent pipes
        where
            newReachable = Set.union reachable current
            newCurrent = Set.fromList [ to | from <- Set.toList current, to <- pipes ! from, Set.notMember to reachable ]

solve2 :: [Programs] -> Pipes -> Int
solve2 programs pipes = go [k | k <- Map.keys pipes, all (Set.notMember k) programs ]
    where
        go [] = length programs
        go (x:_) = solve2 (solve Set.empty (Set.singleton x) pipes:programs) pipes
