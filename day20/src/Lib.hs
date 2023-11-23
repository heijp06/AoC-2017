module Lib
    ( addCollisions
    , collisionTime
    , collisionTime'
    , part1
    , part2
    ) where

import Data.Function (on)
import Data.List (minimumBy, tails)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Particle

type Collisions = Map.Map Int (Set.Set Particle)

part1 :: [String] -> Int
part1 = fst . minimumBy (compare `on` (magnitude . acceleration . snd)) . zip [0..] . map parse
    where
        magnitude [x, y, z] = x * x + y * y + z * z
        magnitude xs = error $ "Illegal acceleration vector: " ++ show xs

part2 :: [String] -> Int
part2 xs = length xs - length collided
    where
        particles = map parse xs
        collisions = foldr addParticles Map.empty (tails particles)
        collided = foldl collide Set.empty . map snd $ Map.toAscList collisions

collide :: Set.Set Particle -> Set.Set Particle -> Set.Set Particle
collide seen toAdd = if length (Set.difference toAdd seen) > 1
                        then Set.union seen toAdd
                        else seen

addParticles :: [Particle] -> Collisions -> Collisions
addParticles [] collisions = collisions
addParticles [_] collisions = collisions
addParticles (x:xs) collisions = foldr (addCollisions x) collisions xs

addCollisions :: Particle -> Particle -> Collisions -> Collisions
addCollisions particle1 particle2 collisions = foldr add collisions $ snd times
    where
        times = foldr1 addTimes $ map (collisionTime particle1 particle2) [0..2]
        addTimes (i1, s1) (i2, s2) =
            if i1
                then if i2 then (True, Set.intersection s1 s2) else (True, s1)
                else (i2, s2)
        add t = Map.insertWith Set.union t (Set.fromList [particle1, particle2])

collisionTime :: Particle -> Particle -> Int -> (Bool, Set.Set Int)
collisionTime particle1 particle2 d = collisionTime' p v a
    where
        p = (position particle1 !! d) - (position particle2 !! d)
        v = (velocity particle1 !! d) - (velocity particle2 !! d)
        a = (acceleration particle1 !! d) - (acceleration particle2 !! d)

collisionTime' :: Int -> Int -> Int -> (Bool, Set.Set Int)
collisionTime' 0 0 0 = (False, Set.empty)
collisionTime' _ 0 0 = (True, Set.empty)
collisionTime' p v 0 = (True, let (t, m) = ((-p) `divMod` v) in collisionTime'' t m)
collisionTime' p v a = (True, if sqrtd * sqrtd == d
                                then Set.union (collisionTime'' t1 m1) (collisionTime'' t2 m2)
                                else Set.empty)
    where
        b = a + 2 * v
        d = b * b - 8 * a * p
        sqrtd = round . sqrt . abs $ fromIntegral d
        (t1, m1) = ((-b) - sqrtd) `divMod` (2 * a)
        (t2, m2) = ((-b) + sqrtd) `divMod` (2 * a)

collisionTime'' :: Int -> Int -> Set.Set Int
collisionTime'' t _ | t <= 0 = Set.empty
collisionTime'' _ m | m /= 0 = Set.empty
collisionTime'' t _ = Set.singleton t