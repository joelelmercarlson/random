#!/usr/bin/env stack
-- stack script --system-ghc --resolver lts-9.18 --package "MonadRandom" --package "text"
module Main where
  import Data.Char
  import Control.Monad (replicateM)
  import Control.Monad.Random
  import System.Environment
  import Text.Printf

  -- | DiceSet "d6" 3 0
  data DiceSet = DiceSet String Int Int deriving (Show)

  dieType :: DiceSet -> String
  dieType (DiceSet x _ _) = x

  dieAmt :: DiceSet -> Int
  dieAmt (DiceSet _ x _) = x

  dieMod :: DiceSet -> Int
  dieMod (DiceSet _ _ x) = x

  -- | Dice
  d1 :: (RandomGen g) => Rand g Int
  d1 = getRandomR (0,1)

  d2 :: (RandomGen g) => Rand g Int
  d2 = getRandomR (1,2)

  d3 :: (RandomGen g) => Rand g Int
  d3 = getRandomR (1,3)

  d4 :: (RandomGen g) => Rand g Int
  d4 = getRandomR (1,4)

  d6 :: (RandomGen g) => Rand g Int
  d6 = getRandomR (1,6)

  d8 :: (RandomGen g) => Rand g Int
  d8 = getRandomR (1,8)

  d10 :: (RandomGen g) => Rand g Int
  d10 = getRandomR (1,10)

  d12 :: (RandomGen g) => Rand g Int
  d12 = getRandomR (1,12)

  d20 :: (RandomGen g) => Rand g Int
  d20 = getRandomR (1,20)

  d100 :: (RandomGen g) => Rand g Int
  d100 = getRandomR (1,100)

  -- | rollers
  d1s :: (RandomGen g) => Int -> Rand g [Int]
  d1s n = replicateM n d1

  d2s :: (RandomGen g) => Int -> Rand g [Int]
  d2s n = replicateM n d2

  d3s :: (RandomGen g) => Int -> Rand g [Int]
  d3s n = replicateM n d3

  d4s :: (RandomGen g) => Int -> Rand g [Int]
  d4s n = replicateM n d4

  d6s :: (RandomGen g) => Int -> Rand g [Int]
  d6s n = replicateM n d6

  d8s :: (RandomGen g) => Int -> Rand g [Int]
  d8s n = replicateM n d8

  d10s :: (RandomGen g) => Int -> Rand g [Int]
  d10s n = replicateM n d10

  d12s :: (RandomGen g) => Int -> Rand g [Int]
  d12s n = replicateM n d12

  d20s :: (RandomGen g) => Int -> Rand g [Int]
  d20s n = replicateM n d20

  d100s :: (RandomGen g) => Int -> Rand g [Int]
  d100s n = replicateM n d100

  -- | generate roll
  -- | Default = 1 d6 0
  modRoll :: [String] -> DiceSet
  modRoll (x:y:z:_) = DiceSet (map toLower y) (read x :: Int) (read z :: Int)
  modRoll []        = DiceSet "d6" 1 0

  zModRoll :: [String] -> DiceSet
  zModRoll (x:y:_) = DiceSet (map toLower y) (read x :: Int) 0
  zModRoll (x:_)   = DiceSet (map toLower x) 1 0
  zModRoll []      = DiceSet "d6" 1 0

  roll :: DiceSet -> IO [Int]
  roll (DiceSet x y _) = case x of
      "d1"   -> evalRandIO $ d1s   y
      "d2"   -> evalRandIO $ d2s   y
      "d3"   -> evalRandIO $ d3s   y
      "d4"   -> evalRandIO $ d4s   y
      "d6"   -> evalRandIO $ d6s   y
      "d8"   -> evalRandIO $ d8s   y
      "d10"  -> evalRandIO $ d10s  y
      "d12"  -> evalRandIO $ d12s  y
      "d20"  -> evalRandIO $ d20s  y
      "d100" -> evalRandIO $ d100s y
      _      -> evalRandIO $ d1s   1

  main :: IO ()
  main = do
    xs <- getArgs
    let ds = if length xs > 2 then modRoll xs else zModRoll xs

    rs <- roll ds
    let t  = sum rs + dm
        dt = dieType ds
        da = dieAmt  ds
        dm = dieMod  ds

    printf "%d%s +/- %d %s = %d\n" da dt dm (show rs) t
