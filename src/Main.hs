module Main where
  import System.Environment
  import DiceSet
  import Display (rpg)
  import Dwarf
  import Elf
  import Hobbit
  import Human
  import ToHit
  import Util (nth)

  main :: IO ()
  main = do
    xs <- getArgs
    r <- roll rollDice

    case nth 1 xs of
      Just "dwarf"        -> dwarf
      Just "elf"          -> elf
      Just "hobbit"       -> hobbit
      Just "human"        -> human
      Just "hit"          -> tohit
      Just "wound"        -> towound
      Just "party"        -> party
      _ -> do { party; tohit; towound }

  party :: IO ()
  party = do
    putStrLn "A Party of Four Adventurers..."
    putStrLn "==============================\n"
    dwarf
    elf
    hobbit
    human

  dwarf :: IO()
  dwarf = do
    r <- roll rollDice
    rpg $ genDwarf r

  elf :: IO()
  elf = do
    r <- roll rollDice
    rpg $ genElf r

  hobbit :: IO()
  hobbit = do
    r <- roll rollDice
    rpg $ genHobbit r

  human :: IO()
  human = do
    r <- roll rollDice
    rpg $ genHuman r

  -- | dice pool
  rollDice :: DiceSet
  rollDice = zModRoll["50", "d10"]
