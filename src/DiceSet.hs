module DiceSet (rollDice, six, ten) where
  import Control.Monad.Random

  type Dice = (Int, Int)

  -- | generate roll
  six :: (RandomGen g) => Rand g Double
  six = do
    randomNumber <- getRandomR(0.0, 1.0)
    return $ randomNumber * 6

  ten :: (RandomGen g) => Rand g Double
  ten = do
    randomNumber <- getRandomR(0.0, 1.0)
    return $ randomNumber * 10

  rollDie :: (RandomGen g) => Int -> Rand g Int
  rollDie sides = getRandomR (1, sides)

  rollDice :: (RandomGen g) => Dice -> Rand g Int
  rollDice (rolls, sides) = sum <$> mapM (const $ rollDie sides) [1..rolls]
