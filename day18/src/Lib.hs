{-# LANGUAGE RecordWildCards #-}

module Lib
    ( part1
    , part2
    ) where

import Control.Monad.State.Lazy (State, evalState, get, put)
import Data.Char (isDigit)
import qualified Data.Map as Map
import Data.List.Split (splitOn)

data Cpu = Cpu { code :: [String]
               , programCounter :: Int
               , registers :: Map.Map Char Int
               }

type Program = State Cpu

part1 :: [String] -> Int
part1 xs = evalState firstRecover Cpu { code = xs
                                      , programCounter = 0
                                      , registers = Map.empty
                                      }

part2 :: [String] -> Int
part2 = undefined

firstRecover :: Program Int
firstRecover = do
    Cpu{..} <- get
    let command = code !! programCounter
    case splitOn command " " of
        ["set", register, number] | (isDigit . head) number -> setNumber (head register) $ read number
        _ -> error "Unknown command"
    return 0

setNumber :: Char -> Int -> Program ()
setNumber register value = do
    cpu@Cpu{..} <- get
    put cpu { registers = Map.insert register value registers }