{-# LANGUAGE RecordWildCards #-}

module Lib
    ( part1
    , part2
    , stress
    ) where

import Control.Monad (replicateM)
import Control.Monad.State.Lazy (State, evalState, execState, put, get)
import Data.Maybe (fromMaybe)

type Position = (Int, Int)

data MemoryLocation = MemoryLocation { square :: Int
                                     , position :: Position
                                     , size :: Int
                                     , values :: [(Position, Int)]
                                     }

start :: MemoryLocation
start = MemoryLocation { square = 1
                       , position = (0, 0)
                       , size = 0
                       , values = [((0, 0), 1)]
                       }

part1 :: Int -> Int
part1 n = abs x + abs y
    where
        (x, y) = position $ execState (replicateM (n - 1) move) start

part2 :: Int -> Int
part2 n = evalState (search n) start

stress :: Int -> Int
stress n = result
    where
        xs = values $ execState (replicateM (n - 1) move) start
        result = snd $ head xs

move :: State MemoryLocation ()
move = do
    MemoryLocation{..} <- get
    case position of
        (x, y) | x == size && y == size -> nextSize
        (x, y) | x == size && y > -size -> nextSquare (x, y - 1)
        (x, y) | y == -size && x > -size -> nextSquare (x - 1, y)
        (x, y) | x == -size && y < size -> nextSquare (x, y + 1)
        (x, y) -> nextSquare (x + 1, y)

nextSize :: State MemoryLocation ()
nextSize = do
    MemoryLocation{..} <- get
    put MemoryLocation { size = size + 1, .. }
    nextSquare (size + 1, size)

nextSquare :: Position -> State MemoryLocation ()
nextSquare pos = do
    MemoryLocation{..} <- get
    put MemoryLocation { square = square + 1, position = pos, values = (pos, getValue pos values) : values, .. }

getValue :: Position -> [(Position, Int)] -> Int
getValue (x, y) values =
    sum [ fromMaybe 0 $ lookup (x + dx, y + dy) values | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0) ]

search :: Int -> State MemoryLocation Int
search n = do
    MemoryLocation{..} <- get
    let (_, v) = head values
    if v > n
        then return v
        else do
            move
            search n
