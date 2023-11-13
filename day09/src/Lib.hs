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
parse = fst . head . readP_to_S (groupParser (1, 0))

groupParser :: (Int, Int) -> ReadP (Int, Int)
groupParser (level, canceled) = do
    _ <- char '{'
    xs <- sepBy (groupParser (level + 1, 0) +++ garbageParser) (char ',')
    _ <- char '}'
    return $ (sum (map fst xs) + level, sum (map snd xs) + canceled)

garbageParser :: ReadP (Int, Int)
garbageParser = do
    _ <- char '<'
    canceled <- length . filter (/='>') <$> many (ignoreParser <++ satisfy (/='>'))
    _ <- char '>'
    return (0, canceled)

ignoreParser :: ReadP Char
ignoreParser = do
    _ <- char '!'
    _ <- get
    return '>'
