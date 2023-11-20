{-# LANGUAGE RecordWildCards #-}

module Lib
    ( part1
    , part2
    ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy (StateT, evalStateT, get, put)
import Data.Char (isLower)
import qualified Data.Map as Map
import Data.List.Split (splitOn)

data Cpu = Cpu { part :: Int
               , code :: [String]
               , programCounter :: Int
               , registers :: Map.Map Char Int
               , sound :: Maybe Int
               , recovered :: Maybe Int
               }

type Program = StateT Cpu (Either String)

part1 :: [String] -> Int
part1 xs = case evalStateT run (newCpu 1 xs) of
            Right value -> value
            Left message -> error message

part2 :: [String] -> Int
part2 = undefined

newCpu :: Int -> [String] -> Cpu
newCpu part xs = Cpu { part = part
                     , code = xs
                     , programCounter = 0
                     , registers = Map.empty
                     , sound = Nothing
                     , recovered = Nothing
                     }

run :: Program Int
run = do
    p <- getPart
    command <- getCommand
    case splitOn " " command of
        ["set", register, value] -> set register value
        ["add", register, value] -> add register value
        ["mul", register, value] -> mul register value
        ["mod", register, value] -> mod' register value
        ["snd", value] -> if p == 1 then snd' value else raise "Part2 is not implemented."
        ["rcv", register] -> if p == 1 then rcv register else raise "Part2 is not implemented."
        ["jgz", register, value] -> jgz register value
        _ -> raise $ "Unknown command: " ++ command
    Cpu{..} <- get
    if p == 1
        then case recovered of
                Just x -> return x
                _ -> run
        else raise "Part 2 is not implemented."

getPart :: Program Int
getPart = do
    Cpu{..} <- get
    return part

getCommand :: Program String
getCommand = do
    cpu@Cpu{..} <- get
    put cpu { programCounter = programCounter + 1 }
    return $ code !! programCounter

getValue :: String -> Program Int
getValue register | (isLower . head) register = do
    Cpu{..} <- get
    let r = head register
    return $ Map.findWithDefault 0 r registers
getValue number = return $ read number

set :: String -> String -> Program ()
set register value = do
    cpu@Cpu{..} <- get
    x <- getValue value
    let r = head register
    put cpu { registers = Map.insert r x registers }

add :: String -> String -> Program ()
add = calcNumber (+)

mul :: String -> String -> Program ()
mul = calcNumber (*)

mod' :: String -> String -> Program ()
mod' = calcNumber (flip mod)

calcNumber :: (Int -> Int -> Int) -> String -> String -> Program ()
calcNumber op register value = do
    Cpu{..} <- get
    x <- getValue value
    let r = head register
    set register $ show(x `op` Map.findWithDefault 0 r registers)

snd' :: String -> Program ()
snd' value = do
    x <- getValue value
    cpu <- get
    put cpu { sound = Just x }

rcv :: String -> Program ()
rcv register = do
    value <- getValue register
    if value == 0
        then
            return ()
        else do
            cpu@Cpu{..} <- get
            put cpu { recovered = sound }

jgz :: String -> String -> Program ()
jgz register value = do
    r <- getValue register
    if r == 0
        then
            return ()
        else do
            v <- getValue value
            cpu@Cpu{..} <- get
            put cpu { programCounter = programCounter + v - 1 }

raise :: String -> Program a
raise message = lift $ Left message