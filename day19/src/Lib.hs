module Lib
    ( part1
    , part2
    ) where

import Data.Map ((!))
import qualified Data.Map as Map

type Position = (Int, Int)

part1 :: [String] -> String
part1 xs = undefined
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
    '|' -> moveOn
    '-' -> moveOn
    '+' -> solve (position `add` newDirection) newDirection grid
    chr -> chr : moveOn
    where
        moveOn = solve (position `add` direction) direction grid
        newDirection = head $ filter (\d -> d /= direction && Map.member (position `add` d) grid) [(1, 0), (0, -1), (-1, 0), (0, 1)]