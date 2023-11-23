module Particle
    ( Particle(..)
    , Vector
    , parse
    ) where

import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

type Vector = [Int]

data Particle = Particle { position :: Vector
                         , velocity :: Vector
                         , acceleration :: Vector
                         } deriving (Eq, Ord, Show)

parse :: String -> Particle
parse = fst . head . readP_to_S particle

particle :: ReadP Particle
particle = do
    p <- property 'p'
    _ <- char ','
    skipSpaces
    v <- property 'v'
    _ <- char ','
    skipSpaces
    a <- property 'a'
    return Particle { position = p, velocity = v, acceleration = a }

property :: Char -> ReadP Vector
property identifier = do
    _ <- char identifier
    _ <- char '='
    vector

vector :: ReadP Vector
vector = do
    _ <- char '<'
    x <- number
    _ <- char ','
    y <- number
    _ <- char ','
    z <- number
    _ <- char '>'
    return [x, y, z]

number :: ReadP Int
number = do
    sign <- option '+' (char '-')
    digits <- many1 $ satisfy isDigit 
    let xs = if sign == '+' then digits else '-':digits
    return $ read xs
