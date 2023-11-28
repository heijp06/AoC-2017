{-# LANGUAGE RecordWildCards #-}

module Lib
    ( part1
    , part2
    ) where

import Control.Monad.State.Lazy (State, execState, get, put)
import qualified Data.Map as Map

type Position = (Int, Int)
type Grid = Map.Map Position NodeState

data NodeState = Clean | Weakened | Infected | Flagged deriving Show

data Virus = Virus { part :: Int
                   , grid :: Grid
                   , position :: Position
                   , direction :: Position
                   , infected :: Int
                   , steps :: Int
                   } deriving Show

part1 :: [String] -> Int
part1 = infected . execState burst . new 1 10000 . parse

part2 :: [String] -> Int
part2 = undefined

new :: Int -> Int -> (Grid, Position) -> Virus
new part steps (grid, start) = Virus { part = part
                                     , grid = grid
                                     , position = start
                                     , direction = (0, -1)
                                     , infected = 0
                                     , steps = steps
                                     }

parse :: [String] -> (Grid, Position)
parse xs = (grid, start)
    where
        height = length xs
        width = length $ head xs
        grid = Map.fromList [ ((x, y), Infected) | y <- [0..height-1], x <- [0..width-1], (xs !! y) !! x == '#' ]
        start = (width `div` 2, height `div` 2)

burst :: State Virus ()
burst = do
    flag <- stop
    if flag
        then return ()
        else do
            turn
            infect
            move
            decrement
            burst

stop :: State Virus Bool
stop = do
    Virus{..} <- get
    return $ steps == 0

turn :: State Virus ()
turn = do
    Virus{..} <- get
    let (x, y) = direction
    if position `Map.member` grid
        then put Virus { direction = (-y, x), .. }
        else put Virus { direction = (y, -x), .. }

infect :: State Virus ()
infect = do
    Virus{..} <- get
    if position `Map.member` grid
        then put Virus { grid = Map.delete position grid, .. }
        else put Virus { grid = Map.insert position Infected grid, infected = infected + 1, .. }


move :: State Virus ()
move = do
    Virus{..} <- get
    let (x, y) = position
    let (dx, dy) = direction
    put Virus { position = (x + dx, y + dy), .. }

decrement :: State Virus ()
decrement = do
    Virus{..} <- get
    put Virus { steps = steps - 1, .. }