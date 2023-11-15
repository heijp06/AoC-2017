module Lib
    ( part1
    , part2
    ) where

type Position = (Int, Int)

part1 :: [String] -> Int
part1 = fst . solve

part2 :: [String] -> Int
part2 = snd . solve

solve :: [String] -> (Int, Int)
solve xs = (distance end, furthest)
    where
        (end, furthest) = foldl add ((0, 0), 0) $ map dirToPos xs
        add ((x1, y1), d) (x2, y2) = let pos = (x1 + x2, y1 + y2) in (pos, max d $ distance pos)

distance :: Position -> Int
distance (x, y) = a + b
    where
        a = abs x `div` 2
        b = max 0 $ (abs y - a) `div` 2

dirToPos :: String -> Position
dirToPos "n" = (0, 2)
dirToPos "ne" = (2, 1)
dirToPos "se" = (2, -1)
dirToPos "s" = (0, -2)
dirToPos "sw" = (-2, -1)
dirToPos "nw" = (-2, 1)
dirToPos xs = error $ "Unknown direction: " ++ xs
