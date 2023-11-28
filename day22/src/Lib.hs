{-# LANGUAGE RecordWildCards #-}

module Lib
    ( part1
    , part2
    ) where

import Control.Monad.State.Lazy (State, execState, get, put)
import qualified Data.Set as Set

type Position = (Int, Int)
type Grid = Set.Set Position

data Virus = Virus { grid :: Grid
                   , position :: Position
                   , direction :: Position
                   , infected :: Int
                   , steps :: Int
                   } deriving Show

part1 :: [String] -> Int
part1 = infected . execState burst . new . parse

part2 :: [String] -> Int
part2 = undefined

new :: (Grid, Position) -> Virus
new (grid, start) = Virus { grid = grid
                          , position = start
                          , direction = (0, -1)
                          , infected = 0
                          , steps = 10000
                          }

parse :: [String] -> (Grid, Position)
parse xs = (grid, start)
    where
        height = length xs
        width = length $ head xs
        grid = Set.fromList [ (x, y) | y <- [0..height-1], x <- [0..width-1], (xs !! y) !! x == '#' ]
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
    if position `Set.member` grid
        then put Virus { direction = (-y, x), .. }
        else put Virus { direction = (y, -x), .. }

infect :: State Virus ()
infect = do
    Virus{..} <- get
    if position `Set.member` grid
        then put Virus { grid = Set.delete position grid, .. }
        else put Virus { grid = Set.insert position grid, infected = infected + 1, .. }


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