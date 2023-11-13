module Lib
    ( part1
    , part2
    ) where

import Text.ParserCombinators.ReadP

part1 :: String -> Int
part1 = fst . head . readP_to_S (groupParser 1)

groupParser :: Int -> ReadP Int
groupParser level = do
    _ <- char '{'
    xs <- sepBy (groupParser (level + 1) +++ garbageParser) (char ',')
    _ <- char '}'
    return $ sum xs + level

garbageParser :: ReadP Int
garbageParser = do
    _ <- char '<'
    _ <- many (ignoreParser <++ satisfy (/='>'))
    _ <- char '>'
    return 0

ignoreParser :: ReadP Char
ignoreParser = do
    _ <- char '!'
    get

part2 :: String -> Int
part2 = undefined
