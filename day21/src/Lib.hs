module Lib
    ( enhance
    , part1
    , part2
    , start
    ) where

import Data.Map ((!))
import Rules

import Debug.Trace (trace)
import Text.Printf (printf)

start :: String
start = ".#...####"

part1 :: [String] -> Int
part1 xs = length $ filter (/='.') step5
    where
        rules = parse xs
        step1 = enhance rules start -- 3 --> 4
        step2 = enhance rules step1 -- 4 --> 6
        step3 = enhance rules step2 -- 6 --> 9
        step4 = enhance rules step3 -- 9 --> 12
        step5 = enhance rules step4 -- 12 --> 18

part2 :: [String] -> Int
part2 = undefined

enhance :: (Rules, Rules) -> String -> String
enhance _ xs | trace (printf "%2d %s" (length xs) xs) False = undefined
enhance (rules2, _) xs | length xs == 4 = rules2 ! xs
enhance (_, rules3) xs | length xs == 9 = rules3 ! xs
enhance rules xs | length xs == 16 = enhance4 rules xs
enhance rules xs | length xs == 36 = enhance6 rules xs
enhance rules xs | length xs == 81 = enhance9 rules xs
enhance rules xs | length xs == 144 = enhance12 rules xs
enhance _ _ = undefined

enhance4 :: (Rules, Rules) -> String -> String
enhance4 rules xs = join 2 3 blocks3
    where
        blocks2 = breakup 2 2 xs
        blocks3 = map (enhance rules) blocks2

enhance6 :: (Rules, Rules) -> String -> String
enhance6 _ xs | trace (show xs) False = undefined
enhance6 rules xs = join 3 3 blocks3
    where
        blocks2 = breakup 3 2 xs
        blocks3 = map (enhance rules) $ trace (show blocks2) blocks2

enhance9 :: (Rules, Rules) -> String -> String
enhance9 rules xs = join 3 4 blocks4
    where
        blocks3 = breakup 3 3 xs
        blocks4 = map (enhance rules) blocks3

enhance12 :: (Rules, Rules) -> String -> String
enhance12 rules xs = join 6 3 blocks3
    where
        blocks2 = breakup 6 2 xs
        blocks3 = map (enhance rules) blocks2