module Lib
    ( part1
    , part2
    ) where

import Data.Function (on)
import Data.List (minimumBy)
import Particle

part1 :: [String] -> Int
part1 = fst . minimumBy (compare `on` (magnitude . acceleration . snd)) . zip [0..] . map parse
    where
        magnitude (x, y, z) = x * x + y * y + z * z

part2 :: [String] -> Int
part2 = undefined
