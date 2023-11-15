module Lib
    ( part1
    , part2
    ) where

type Position = (Int, Int)

part1 :: [String] -> Int
part1 xs = a + b
    where
        (x, y) = foldr (add . dirToPos) (0, 0) xs
        a = abs x `div` 2
        b = max 0 $ (abs y - a) `div` 2

part2 :: [String] -> Int
part2 = undefined

dirToPos :: String -> Position
dirToPos "n" = (0, 2)
dirToPos "ne" = (2, 1)
dirToPos "se" = (2, -1)
dirToPos "s" = (0, -2)
dirToPos "sw" = (-2, -1)
dirToPos "nw" = (-2, 1)
dirToPos xs = error $ "Unknown direction: " ++ xs

add :: Position -> Position -> Position
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
