module Lib
    ( part1
    , part2
    ) where

import qualified Data.Map as Map

type Position = (Int, Int)

part1 :: [String] -> String
part1 xs = solve start (0, 1) grid
    where
        start = head . filter ((==0) . snd) $ Map.keys grid
        grid = Map.fromList . filter ((/=' ') . snd) $ zip (flip (,) <$> [0..height-1] <*> [0..width-1]) (concat xs)
        height = length xs
        width = (length . head) xs

part2 :: [String] -> String
part2 = undefined

add :: Position -> Position -> Position
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

solve :: Position -> Position -> Map.Map Position Char -> String
solve position direction grid = case Map.findWithDefault ' ' position grid of
    ' ' -> ""
    '|' -> move direction
    '-' -> move direction
    '+' -> move newDirection
    chr -> chr : move direction
    where
        move d = solve (position `add` d) d grid
        newDirection = head $ filter ((`Map.member` grid) . (add position)) options
        options = let (x, y) = direction in [(y, x), (-y, -x)]