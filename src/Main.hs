module Main where
  import System.Environment
  import Text.Printf

  import Character
  import DiceSet
  import Display (rpg, rpg')
  import Dwarf
  import Elf
  import Hobbit
  import Human
  import ToHit
  import Util (nth)

  main :: IO ()
  main = do
    xs <- getArgs

    let x  = zModRoll ["25", "d10"]
        y  = zModRoll ["10", "d20"]
        ds = if length xs > 2 then modRoll xs else zModRoll xs

    r0 <- roll ds
    r1 <- roll x
    r2 <- roll y

    case nth 1 xs of
      (Just "dwarf")        -> rpg $                genDwarf r1 r2
      (Just "greybeard")    -> rpg $ greybeard    $ genDwarf r1 r2
      (Just "dragonseeker") -> rpg $ dragonseeker $ genDwarf r1 r2
      (Just "elf")     -> rpg  $ genElf      r1 r2
      (Just "elf2")    -> rpg' $ genElf      r1 r2
      (Just "hobbit")  -> rpg  $ genHalfling r1 r2
      (Just "hobbit2") -> rpg' $ genHalfling r1 r2
      (Just "human")   -> rpg  $ genHuman    r1 r2
      (Just "human2")  -> rpg' $ genHuman    r1 r2
      (Just "hit")     -> tohit
      (Just "wound")   -> towound
      otherwise        -> printf "%d %s +/- %d %s = %d\n" (dieAmt ds) (dieType ds) (dieMod ds) (show r0) ((sum r0) + (dieMod ds))
