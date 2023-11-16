module Lib
    ( part1
    , part2
    ) where

-- Hexagonal positions around the origin in the complex plane are at exp(i*pi*(1/2 - k/3))
-- (k = 0: "n", k = 1: "ne", k = 2: "se", k = 3: "s", k = 4: "sw", k = 5: "nw").
-- Of these "n" (i) and "s" (-i) have real part 0.
-- The other directions have all four combinations of Re = +/- sqrt(3)/2 and Im = +/- 1/2
-- Scaling the x direction by dividing by sqrt(3)/2 and
-- scaling the y direction by multiplying by 2 gives the table in dirToPos.
-- Combining distances is done by adding corresponding tuple elements.
-- The shortest distance to get to (0, 0) from (x, y) is found by:
-- 1. Taking the minimum steps needed to get to x == 0: abs x
-- 2. Because we have all combinations of x == +/- 1 and y == +/- 1: If abs x >= abs y, it is
--    always possible to always get to y == 0 while doing 1. Else the minimal amount of steps
--    needed to get to y == 0 is (abs x - abs y) `div` 2. (The `div` 2 is because "n" and "s"
--    have abs y == 2.)
-- 3. Combining 1. and 2. gives the formula for the distance:
--    abs x + max 0 ((abs y - abs x) `div` 2)

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
distance (x, y) = abs x + max 0 ((abs y - abs x) `div` 2)

dirToPos :: String -> Position
dirToPos "n" = (0, 2)
dirToPos "ne" = (1, 1)
dirToPos "se" = (1, -1)
dirToPos "s" = (0, -2)
dirToPos "sw" = (-1, -1)
dirToPos "nw" = (-1, 1)
dirToPos xs = error $ "Unknown direction: " ++ xs
