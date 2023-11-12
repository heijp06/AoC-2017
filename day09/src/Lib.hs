{-# LANGUAGE RecordWildCards #-}

module Lib
    ( part1
    , part2
    ) where

data State = State { level :: Int
                   , total :: Int
                   , stream :: String
                   }

part1 :: String -> Int
part1 xs = total $ group State { level = 1
                               , total = 0
                               , stream = xs
                               }

part2 :: String -> Int
part2 = undefined

group :: State -> State
group state@State{..} | head stream == '}' = state
group state@State{..} = group state { level = level + 1, total = total + level, stream = tail stream }