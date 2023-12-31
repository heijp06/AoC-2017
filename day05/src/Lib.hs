{-# LANGUAGE RecordWildCards #-}

module Lib
    ( part1
    , part2
    ) where

import Control.Monad.State.Lazy (State, evalState, get, put)
import Data.Map (Map, (!), adjust, fromList)

data Cpu = Cpu { code :: Map Int Int
               , index :: Int
               , steps :: Int
               }

type Updater = Int -> Int

part1 :: [Int] -> Int
part1 = solve (+1)

part2 :: [Int] -> Int
part2 = solve $ \x -> if x >= 3 then x - 1 else x + 1

solve :: Updater -> [Int] -> Int
solve updater xs = evalState (run updater) Cpu { code = fromList $ zip [0..] xs
                                               , index = 0
                                               , steps = 0
                                               }

run :: Updater -> State Cpu Int
run updater = do
    Cpu{..} <- get
    let newSteps = steps + 1
    let offset = code ! index
    let newIndex = index + offset
    if newIndex < 0 || newIndex >= length code
        then return newSteps
        else do
            let newCode = adjust updater index code
            put Cpu { code = newCode, index = newIndex, steps = newSteps }
            run updater