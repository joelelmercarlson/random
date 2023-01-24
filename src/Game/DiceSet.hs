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
d2 s = fromIntegral $ rollMod 1 2 0 s

d3 :: Int -> Int
d3 s = fromIntegral $ rollMod 1 3 0 s

d4 :: Int -> Int
d4 s = fromIntegral $ rollMod 1 4 0 s

d5 :: Int -> Int
d5 s = fromIntegral $ rollMod 1 5 0 s

d6 :: Int -> Int
d6 s = fromIntegral $ rollMod 1 6 0 s

d8 :: Int -> Int
d8 s = fromIntegral $ rollMod 1 8 0 s

d10 :: Int -> Int
d10 s = fromIntegral $ rollMod 1 10 0 s

d12 :: Int -> Int
d12 s = fromIntegral $ rollMod 1 12 0 s

d20 :: Int -> Int
d20 s = fromIntegral $ rollMod 1 20 0 s

d50 :: Int -> Int
d50 s = fromIntegral $ rollMod 1 50 0 s

d100 :: Int -> Int
d100 s = fromIntegral $ rollMod 1 100 0 s

-- | initial seed roll for the World
d1000 :: RandomGen g => g -> (Int, g)
d1000 gen = let
  (r, g) = randomR (1, 1000) gen
  in (r, g)

-- | roll w/ seed
roll :: Int -> Word -> Int -> [Word]
roll r side seed = let
  rolls :: RandomGen g => Int -> Word -> g -> [Word]
  rolls n s = take n . unfoldr (Just . uniformR (1,s))
  pureGen = mkStdGen seed
  in rolls r side pureGen :: [Word]

-- | rollList w/ seed
rollList :: Int -> Word -> Int -> [Int]
rollList n side seed = let
  rs = roll n side seed
  in [ i | r <- rs, let i = fromIntegral r ]

-- | rollMod w/ seed
-- example: D20 + 1
-- rollMod 1 20 + 1
rollMod :: Int -> Word -> Word -> Int -> Word
rollMod r side n seed = let
  rs = sum $ roll r side seed
  in rs + n
