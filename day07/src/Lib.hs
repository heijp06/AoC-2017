module Lib
    ( part1
    , part2
    ) where

import Data.Map ((!))
import qualified Data.Map as M

data Tree = Node String Int [Tree] deriving Show

part1 :: [String] -> String
part1 xs = head [ x | x <- bottoms (map words xs), x `notElem` tops (map words xs) ]

part2 :: [String] -> Int
part2 = undefined

bottoms :: [[String]] -> [String]
bottoms = map head

tops :: [[String]] -> [String]
tops = map (filter (/=',')) . concatMap (drop 3)

buildTree :: M.Map String (Int, [String]) -> String -> Tree
buildTree nodes xs = let t = nodes ! xs in Node xs (fst t) $ map (buildTree nodes) (snd t)

buildNodes :: [String] -> M.Map String (Int, [String])
buildNodes xs = M.fromList [ (head $ words x, (read . head . tail $ words x, map (filter (/=',')) . drop 3 $ words x)) | x <- xs ]