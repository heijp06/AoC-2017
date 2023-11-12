module Lib
    ( part1
    , part2
    ) where

import Data.Function (on)
import Data.List (group, maximumBy, nub, sort)
import Data.List.Extra (allSame)
import Data.Map ((!))
import qualified Data.Map as M

data Tree = Node String Int [Tree] deriving Show

part1 :: [String] -> String
part1 xs = head [ x | x <- bottoms (map words xs), x `notElem` tops (map words xs) ]

part2 :: [String] -> Int
part2 xs = balance $ children tree
    where
        tree = buildTree (buildNodes xs) (part1 xs)

balance :: [Tree] -> Int
balance [t1, t2] = if isBalanced t1 then balance (children t2) else balance (children t1)
balance ts = if allSame (map weight ts') then expectedWeight - sum (map weight ts') else balance ts'
    where
        expectedWeight = head . maximumBy (compare `on` length) . group . sort $ map weight ts
        unbalanced = head $ filter (\t -> weight t /= expectedWeight) ts
        ts' = children unbalanced

children :: Tree -> [Tree]
children (Node _ _ ts) = ts

weight :: Tree -> Int
weight (Node _ w ts) = w + sum [ weight t | t <- ts ]

isBalanced :: Tree -> Bool
isBalanced (Node _ _ ts) = length (nub [ weight t | t <- ts ]) == 1

bottoms :: [[String]] -> [String]
bottoms = map head

tops :: [[String]] -> [String]
tops = map (filter (/=',')) . concatMap (drop 3)

buildTree :: M.Map String (Int, [String]) -> String -> Tree
buildTree nodes xs = let t = nodes ! xs in Node xs (fst t) $ map (buildTree nodes) (snd t)

buildNodes :: [String] -> M.Map String (Int, [String])
buildNodes xs = M.fromList [ (head $ words x, (read . head . tail $ words x, map (filter (/=',')) . drop 3 $ words x)) | x <- xs ]