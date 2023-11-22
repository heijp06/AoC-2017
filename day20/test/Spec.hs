import Test.Hspec hiding (example)

import Lib
import Particle

line :: String
line = "p=<-1021,-2406,1428>, v=<11,24,-73>, a=<4,9,0>"

particle :: Particle
particle = Particle { position = (-1021, -2406, 1428)
                    , velocity = (11, 24, -73)
                    , acceleration = (4, 9, 0)
                    }

example :: [String]
example = [ "p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>"
          , "p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>"
          ]

main :: IO ()
main = hspec $ do
    describe "Parse" $ do
        it "parse line -> Particle" $ parse line `shouldBe` particle
    
    describe "Part 1" $ do
        it "part1 example -> 0" $ part1 example `shouldBe` 0
