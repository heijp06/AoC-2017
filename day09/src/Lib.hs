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
group level ('}':xs) = (level, xs)
group level ('{':xs) = let (result, ys) = group (level + 1) xs in (level + result, ys)
group level [] = error $ "Unexpected end of string in group, level = " ++ show level
group level xs = error $ "Unexpected characters '" ++ xs ++ "' in group, level = " ++ show level
