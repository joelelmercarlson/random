{-

Game.DiceSet.hs

Game.DiceSet rolls dice

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.DiceSet where

import Control.Monad.Random
import Data.List
import System.Random

-- | bag of dice
d1 :: Int -> Int
d1 _ = 1

d2 :: Int -> Int
d2 = uniformRoll 2

d3 :: Int -> Int
d3 = uniformRoll 3

d4 :: Int -> Int
d4 = uniformRoll 4

d5 :: Int -> Int
d5 = uniformRoll 5

d6 :: Int -> Int
d6 = uniformRoll 6

d8 :: Int -> Int
d8 = uniformRoll 8

d10 :: Int -> Int
d10 = uniformRoll 10

d12 :: Int -> Int
d12 = uniformRoll 12

d20 :: Int -> Int
d20 = uniformRoll 20

d100 :: Int -> Int
d100 = uniformRoll 100

-- | initial seed roll for the World
d1000 :: RandomGen g => g -> (Int, g)
d1000 = randomR (1, 1000)

-- | roll r d(x)
roll :: Int -> Int -> Int -> [Word]
roll r x seed = let
  rolls :: RandomGen g => Int -> Word -> g -> [Word]
  rolls n s = take n . unfoldr (Just . uniformR (1, s))
  side    = fromIntegral x :: Word
  pureGen = mkStdGen seed
  in rolls r side pureGen :: [Word]

-- | uniformRoll 1d(x)
uniformRoll :: Int -> Int -> Int
uniformRoll x s = head $ uniformRolls 1 x s

-- | rollList - n side seed rolls
uniformRolls :: Int -> Int -> Int -> [Int]
uniformRolls n side seed = map fromIntegral $ roll n side seed
