module Lib
    ( part1
    ) where

import Data.Char(digitToInt)
part1 :: String -> Int
part1 xs = sum $ [ a | (a, b) <- zip digits (tail digits), a == b ]
    where
        digits = [ digitToInt c | c <- xs ++ [head xs] ]
