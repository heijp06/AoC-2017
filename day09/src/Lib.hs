module Lib
    ( part1
    , part2
    ) where

type State = (Int, String)

part1 :: String -> Int
part1 xs = fst . group 1 $ tail xs

part2 :: String -> Int
part2 = undefined

group :: Int -> String -> State
group level xs = (level, tail xs)