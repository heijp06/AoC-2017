module Rules ( Rules
             , create
             , flip3
             , parse
             , rotate2
             , rotate3
             ) where

import Data.List (partition)
import Data.List.Split (splitOn)
import qualified Data.Map as Map

type Rules = Map.Map String String

parse :: [String] -> (Rules, Rules)
parse xs = undefined
    where
        (rules2, rules3) = partition ((>20) . length) xs

create :: String -> (String, String)
create xs = case splitOn " => " $ filter (/= '/') xs of
                [k, v] -> (k, v)
                _ -> error $ "Invalid rule: " ++ xs

rotate2 :: String -> String
rotate2 = transform [2, 0, 3, 1]

rotate3 :: String -> String
rotate3 = transform [6, 3, 0, 7, 4, 1, 8, 5, 2]

flip3 :: String -> String
flip3 = transform [2, 1, 0, 5, 4, 3, 8, 7, 6]

transform :: [Int] -> String -> String
transform is xs = map (xs !!) is