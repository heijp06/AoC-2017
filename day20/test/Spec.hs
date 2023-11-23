import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.Hspec hiding (example)

import Lib
import Particle

line :: String
line = "p=<-1021,-2406,1428>, v=<11,24,-73>, a=<4,9,0>"

particle :: Particle
particle = Particle { position = [-1021, -2406, 1428]
                    , velocity = [11, 24, -73]
                    , acceleration = [4, 9, 0]
                    }

particle1 :: Particle
particle1 = parse $ head example2

particle2 :: Particle
particle2 = parse $ example2 !! 1

example :: [String]
example = [ "p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>"
          , "p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>"
          ]

example2 :: [String]
example2 = [ "p=<-6,0,0>, v=<3,0,0>, a=<0,0,0>"
           , "p=<-4,0,0>, v=<2,0,0>, a=<0,0,0>"
           , "p=<-2,0,0>, v=<1,0,0>, a=<0,0,0>"
           , "p=<3,0,0>, v=<-1,0,0>, a=<0,0,0>"
           ]

main :: IO ()
main = hspec $ do
    describe "Parse" $ do
        it "parse line -> Particle" $ parse line `shouldBe` particle
    
    describe "Part 1" $ do
        it "part1 example -> 0" $ part1 example `shouldBe` 0

    describe "Part 2" $ do
        it "part2 example -> 1" $ part2 example2 `shouldBe` 1

    describe "collisionTime'" $ do
        it "collisionTime' -2 1 0 -> {2}" $ collisionTime' (-2) 1 0 `shouldBe` (True, Set.singleton 2)
        it "collisionTime' 0 0 0 -> False" $ collisionTime' 0 0 0 `shouldBe` (False, Set.empty)
    
    describe "collisionTime" $ do
        it "collisionTime particle1 particle2 x -> 2" $
            collisionTime particle1 particle2 0 `shouldBe` (True, Set.singleton 2)
        it "collisionTime particle1 particle2 y -> False" $
            collisionTime particle1 particle2 1 `shouldBe` (False, Set.empty)
        it "collisionTime particle1 particle2 z -> False" $
            collisionTime particle1 particle2 2 `shouldBe` (False, Set.empty)
    
    describe "addCollisions" $ do
        it "addCollisions particle1 particle2 -> 2" $
            addCollisions particle1 particle2 Map.empty `shouldBe` Map.singleton 2 (Set.fromList [particle1, particle2])