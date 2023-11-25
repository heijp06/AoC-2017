{-# LANGUAGE TupleSections #-}

module Rules ( Rules
             , breakup
             , create
             , flip2
             , flip3
             , indexes
             , indexesBreakup
             , join
             , parse
             , rotate2
             , rotate3
             ) where

import Prelude hiding (flip)
import Data.List (partition)
import Data.List.Split (chunksOf, splitOn)
import qualified Data.Map as Map

type Rules = Map.Map String String
type Transformation = String -> String

parse :: [String] -> (Rules, Rules)
parse xs = ( Map.unions $ map (create rotate2 flip2) rules2
           , Map.unions $ map (create rotate3 flip3) rules3
           )
    where
        (rules2, rules3) = partition ((<21) . length) xs

create :: Transformation -> Transformation -> String -> Rules
create rotate flip xs = case splitOn " => " $ filter (/= '/') xs of
                [k, v] -> Map.fromList . map (,v) $ keys rotate flip k
                _ -> error $ "Invalid rule: " ++ xs

keys :: Transformation -> Transformation -> String -> [String]
keys rotate flip xs = build xs ++ build (flip xs)
    where
        build ys = take 4 $ iterate rotate ys

rotate2 :: Transformation
rotate2 = transform [2, 0, 3, 1]

flip2 :: Transformation
flip2 = transform [1, 0, 3, 2]

rotate3 :: Transformation
rotate3 = transform [6, 3, 0, 7, 4, 1, 8, 5, 2]

flip3 :: Transformation
flip3 = transform [2, 1, 0, 5, 4, 3, 8, 7, 6]

transform :: [Int] -> Transformation
transform is xs = map (xs !!) is

breakup :: Int -> Int -> String -> [String]
breakup bn n xs = chunksOf (n * n) $ (`transform` xs) (indexes bn n)

join :: Int -> Int -> [String] -> String
join bn n xss = (`transform` concat xss) (indexes bn n)

indexes :: Int -> Int -> [Int]
indexes bn n = [ by * bn * n * n + y * n + bx * n * n + x | by <- bns, y <- ns, bx <- bns, x <- ns ]
    where
        bns = [0..bn-1]
        ns = [0..n-1]

indexesBreakup :: Int -> Int -> [Int]
indexesBreakup bn n = [ by * bn * n * n + bx * n + y * bn * n + x | by <- bns, bx <- bns, y <- ns, x <- ns ]
    where
        bns = [0..bn-1]
        ns = [0..n-1]