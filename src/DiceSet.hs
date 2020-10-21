module DiceSet (d6, d10, rollDice) where
  import Control.Monad.Random

  type Dice = (Int, Int)

  -- | generate roll
  d6 :: (RandomGen g) => Rand g Double
  d6 = do
    randomNumber <- getRandomR(0.0, 1.0)
    return $ randomNumber * 6

  d10 :: (RandomGen g) => Rand g Double
  d10 = do
    randomNumber <- getRandomR(0.0, 1.0)
    return $ randomNumber * 10

  rollDie :: (RandomGen g) => Int -> Rand g Int
  rollDie sides = getRandomR (1, sides)

  rollDice :: (RandomGen g) => Dice -> Rand g Int
  rollDice (rolls, sides) = sum <$> mapM (const $ rollDie sides) [1..rolls]
