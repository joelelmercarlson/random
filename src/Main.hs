module Main where
  import System.Environment
  import DiceSet (d10, d100)
  import Display (rpg)
  import Elf
  import Dwarf
  import Human
  import Hobbit
  import ToHit
  import Util (nth)

  main :: IO ()
  main = do
    xs <- getArgs

    case nth 1 xs of
      Just "elf" -> do { e <- genElf; rpg e }
      Just "dwarf" -> do { e <- genDwarf; rpg e }
      Just "human" -> do { e <- genHuman; rpg e }
      Just "hobbit" -> do { e <- genHobbit; rpg e }
      Just "party" -> party
      Just "hit" -> tohit
      Just "wound" -> towound
      Just "d10" -> do { d <- d10; print $ d}
      Just "d100" -> do { d <- d100; print $ d}
      _ -> do { party }

  party :: IO ()
  party = do
    e <- genElf
    d <- genDwarf
    h <- genHuman
    r <- genHobbit
    putStrLn "A Party of Four Adventurers..."
    putStrLn "=============================="
    rpg $ e
    rpg $ d
    rpg $ h
    rpg $ r
