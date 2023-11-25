module Rules ( Rules
             , flip3
             , parse
             , rotate2
             , rotate3
             ) where

import qualified Data.Map as Map

type Rules = Map.Map String String

parse :: [String] -> (Rules, Rules)
parse = undefined

rotate2 :: String -> String
rotate2 = transform [2, 0, 3, 1]

rotate3 :: String -> String
rotate3 = transform [6, 3, 0, 7, 4, 1, 8, 5, 2]

flip3 :: String -> String
flip3 = transform [2, 1, 0, 5, 4, 3, 8, 7, 6]

transform :: [Int] -> String -> String
transform is xs = map (xs !!) is