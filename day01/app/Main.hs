module Main (main) where

import Lib (part1)

main :: IO ()
main = do
    input <- readFile "..\\..\\2017-data\\day01.txt"
    print $ part1 $ head (lines input)
