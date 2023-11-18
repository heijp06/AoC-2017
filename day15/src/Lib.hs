module Lib
    ( part1
    , part2
    ) where

part1 :: Int -> Int -> Int
part1 a b = sum [ 1 | (u, v) <- zip generatorA generatorB, (u `mod` 65536) == (v `mod` 65536) ]
    where
        generatorA = generator a 16807
        generatorB = generator b 48271
        generator start factor = take 40000000 . drop 1 $ iterate ((`mod` 2147483647) . (*factor)) start

part2 :: Int -> Int -> Int
part2 a b = sum [ 1 | (u, v) <- zip generatorA generatorB, (u `mod` 65536) == (v `mod` 65536) ]
    where
        generatorA = take 5000000 . filter ((==0) . (`mod` 4)) $ generator a 16807
        generatorB = take 5000000 . filter ((==0) . (`mod` 8)) $ generator b 48271
        generator start factor = drop 1 $ iterate ((`mod` 2147483647) . (*factor)) start
