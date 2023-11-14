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
groupParser (score, nonCanceled) = do
    _ <- char '{'
    xs <- sepBy (groupParser (score + 1, 0) +++ garbageParser) (char ',')
    _ <- char '}'
    return $ (sum (map fst xs) + score, sum (map snd xs) + nonCanceled)

garbageParser :: ReadP (Int, Int)
garbageParser = do
    _ <- char '<'
    nonCanceled <- sum <$> many (ignoreParser <++ (const 1 <$> satisfy (/='>')))
    _ <- char '>'
    return (0, nonCanceled)

ignoreParser :: ReadP Int
ignoreParser = do
    _ <- char '!'
    _ <- get
    return 0
