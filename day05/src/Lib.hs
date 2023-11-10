{-# LANGUAGE RecordWildCards #-}

module Lib
    ( part1
    , part2
    ) where

import Control.Monad.State.Lazy (State, evalState, get, put)

data Cpu = Cpu { code :: [Int]
               , index :: Int
               , steps :: Int
               }

part1 :: [Int] -> Int
part1 xs = evalState run Cpu { code = xs
                             , index = 0
                             , steps = 0
                             }

part2 :: [Int] -> Int
part2 = undefined

run :: State Cpu Int
run = do
    Cpu{..} <- get
    let newSteps = steps + 1
    let offset = code !! index
    let newIndex = index + offset
    if newIndex < 0 || newIndex >= length code
        then return newSteps
        else do
            let newCode = take index code ++ [offset + 1] ++ drop (index + 1) code
            put Cpu { code = newCode, index = newIndex, steps = newSteps }
            run