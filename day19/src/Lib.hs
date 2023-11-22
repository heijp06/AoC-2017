module Lib
    ( part1
    , part2
    ) where

import qualified Data.Map as Map

type Position = (Int, Int)

part1 :: [String] -> String
part1 = fst . solve

part2 :: [String] -> Int
part2 = snd . solve

solve :: [String] -> (String, Int)
solve xs = go start (0, 1) grid
    where
        start = head . filter ((==0) . snd) $ Map.keys grid
        grid = Map.fromList . filter ((/=' ') . snd) $ zip (flip (,) <$> [0..height-1] <*> [0..width-1]) (concat xs)
        height = length xs
        width = (length . head) xs

go :: Position -> Position -> Map.Map Position Char -> (String, Int)
go position direction grid = case Map.findWithDefault ' ' position grid of
    ' ' -> ("", 0)
    '|' -> move "" direction
    '-' -> move "" direction
    '+' -> move "" newDirection
    chr -> move [chr] direction
    where
        move x d = let (y, n) = go (position `add` d) d grid in (x ++ y, n + 1)
        newDirection = head $ filter ((`Map.member` grid) . (add position)) options
        options = let (x, y) = direction in [(y, x), (-y, -x)]

add :: Position -> Position -> Position
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)