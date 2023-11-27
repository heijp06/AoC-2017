module Lib
    ( enhance
    , part1
    , part2
    , start
    ) where

import Data.Map ((!))
import qualified Data.Map as Map
import Rules

start :: String
start = ".#...####"

part1 :: [String] -> Int
part1 xs = length $ filter (=='#') step5
    where
        rules = parse xs
        step1 = enhance rules start -- 3 --> 4
        step2 = enhance rules step1 -- 4 --> 6
        step3 = enhance rules step2 -- 6 --> 9
        step4 = enhance rules step3 -- 9 --> 12
        step5 = enhance rules step4 -- 12 --> 18

part2 :: [String] -> Int
part2 xs = sum $ map calculate (Map.toList result)
    where
        rules = parse xs
        result = last . take 7 . iterate (enhanceAll rules) $ Map.singleton start 1
        calculate (ys, count) = length (filter (=='#') ys) * count

enhanceAll :: (Rules, Rules) -> Map.Map String Int -> Map.Map String Int
enhanceAll rules counts = foldr (add rules) Map.empty $ Map.toList counts

add :: (Rules, Rules) -> (String, Int) -> Map.Map String Int -> Map.Map String Int
add rules (xs, count) counts = foldr (add' count) counts $ threeSteps rules xs

add' :: Int -> String -> Map.Map String Int -> Map.Map String Int
add' count xs = Map.insertWith (+) xs count

threeSteps :: (Rules, Rules) -> String -> [String]
threeSteps rules xs = breakup 3 3 step3
    where
        step1 = enhance rules xs -- 3 --> 4
        step2 = enhance rules step1 -- 4 --> 6
        step3 = enhance rules step2 -- 6 --> 9

enhance :: (Rules, Rules) -> String -> String
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
enhance6 rules xs = join 3 3 blocks3
    where
        blocks2 = breakup 3 2 xs
        blocks3 = map (enhance rules) $ blocks2

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