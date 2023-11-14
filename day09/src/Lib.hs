{-# LANGUAGE TupleSections #-}

module Lib
    ( part1
    , part2
    ) where

import Text.ParserCombinators.ReadP

part1 :: String -> Int
part1 = fst . parse

part2 :: String -> Int
part2 = snd . parse

parse :: String -> (Int, Int)
parse = fst . head . readP_to_S (group 1)

group :: Int -> ReadP (Int, Int)
group score = do
    _ <- char '{'
    xs <- (group (score + 1) +++ ((0,) <$> garbage)) `sepBy` char ','
    _ <- char '}'
    return (sum (map fst xs) + score, sum (map snd xs))

garbage :: ReadP Int
garbage = do
    _ <- char '<'
    nonCanceled <- sum <$> many (ignore <++ (const 1 <$> satisfy (/='>')))
    _ <- char '>'
    return nonCanceled

ignore :: ReadP Int
ignore = do
    _ <- char '!'
    _ <- get
    return 0
