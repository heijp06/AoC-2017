{-# LANGUAGE RecordWildCards #-}

module Lib
    ( part1
    , part2
    , stress
    ) where

import Control.Monad (replicateM)
import Control.Monad.State.Lazy (State, execState, put, get)

type Position = (Int, Int)

data MemoryLocation = MemoryLocation { square :: Int
                                     , position :: Position
                                     , size :: Int
                                     }

start :: MemoryLocation
start = MemoryLocation { square = 1
                       , position = (0, 0)
                       , size = 0
                       }

part1 :: Int -> Int
part1 n = abs x + abs y
    where
        (x, y) = position $ execState (replicateM (n - 1) move) start

part2 = undefined

stress :: Int -> Int
stress = undefined

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
    put MemoryLocation { square = square + 1, position = pos, .. }