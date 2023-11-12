{-# LANGUAGE RecordWildCards #-}

module Lib
    ( execute
    , part1
    , part2
    ) where

import qualified Data.Map as M

data Command = Command { r1 :: String
                       , r2 :: String
                       , op :: Int -> Int 
                       , comp :: Int -> Bool
                       }

part1 :: [String] -> Int
part1 = maximum . M.elems . foldl execute M.empty

execute :: M.Map String Int -> String -> M.Map String Int
execute registers line = go $ parse line
    where
        go Command{..} = if comp (get r2)
                            then M.insert r1 (op $ get r1) registers
                            else registers
        get r = M.findWithDefault 0 r registers

parse :: String -> Command
parse xs = Command { r1 = head ws
                   , r2 = ws !! 4
                   , op = buildOp (ws !! 1) (ws !! 2)
                   , comp = buildComp (ws !! 5) (ws !! 6)
                   }
    where
        ws = words xs
        buildOp "inc" x = let v = (read x :: Int) in (+v)
        buildOp "dec" x = let v = (read x :: Int) in subtract v
        buildOp op _ = error $ "Unknown operation: " ++ op
        buildComp "<" x = let v = (read x :: Int) in (<v)
        buildComp "<=" x = let v = (read x :: Int) in (<=v)
        buildComp ">" x = let v = (read x :: Int) in (>v)
        buildComp ">=" x = let v = (read x :: Int) in (>=v)
        buildComp "==" x = let v = (read x :: Int) in (==v)
        buildComp "!=" x = let v = (read x :: Int) in (/=v)
        buildComp comp _ = error $ "Unknown comparison: " ++ comp

part2 :: [String] -> Int
part2 = undefined