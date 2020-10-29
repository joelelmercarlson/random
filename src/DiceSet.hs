module DiceSet (d4
               ,d6
               , d8
               , d10
               , d12
               , d20
               , threeD6
               , twoD10
               , d50
               , d100) where
  import Control.Monad.Random

  type Dice = (Int, Int)

  -- | various dice
  d4 :: IO Int
  d4 = do
    d <- evalRandIO (rollDice (1, 4))
    return $ d

  d6 :: IO Int
  d6 = do
    d <- evalRandIO (rollDice (1, 6))
    return $ d

  d8 :: IO Int
  d8 = do
    d <- evalRandIO (rollDice (1, 8))
    return $ d

  d10 :: IO Int
  d10 = do
    d <- evalRandIO (rollDice (1, 10))
    return $ d

  d12 :: IO Int
  d12 = do
    d <- evalRandIO (rollDice (1, 12))
    return $ d

  d20 :: IO Int
  d20 = do
    d <- evalRandIO (rollDice (1, 20))
    return $ d

  threeD6 :: IO Int
  threeD6 = do
    d <- evalRandIO (rollDice (3, 6))
    return $ d

  twoD10 :: IO Int
  twoD10 = do
    d <- evalRandIO (rollDice (2, 10))
    return $ d

  d50 :: IO Int
  d50 = do
    d <- evalRandIO (rollDice (5, 10))
    return $ d

  d100 :: IO Int
  d100 = do
    d <- evalRandIO (rollDice (1, 100))
    return $ d

  -- | dice rollers
  rollDie :: (RandomGen g) => Int -> Rand g Int
  rollDie sides = getRandomR (1, sides)

  rollDice :: (RandomGen g) => Dice -> Rand g Int
  rollDice (rolls, sides) = sum <$> mapM (const $ rollDie sides) [1..rolls]
