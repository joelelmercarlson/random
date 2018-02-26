module Main where
  import System.Environment
  import Text.Printf

  import Character
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

    let x  = zModRoll ["100", "d10"]
        ds = if length xs > 2 then modRoll xs else zModRoll xs

    r0 <- roll ds
    r1 <- roll x

    case nth 1 xs of
      (Just "dwarf")        -> rpg $ genDwarf  r1
      (Just "elf")          -> rpg $ genElf    r1
      (Just "hobbit")       -> rpg $ genHobbit r1
      (Just "human")        -> rpg $ genHuman  r1
      (Just "king")         -> rpg $ king         $ genDwarf r1
      (Just "thane")        -> rpg $ thane        $ genDwarf r1
      (Just "dragonseeker") -> rpg $ dragonSeeker $ genDwarf r1
      (Just "runicsmith")   -> rpg $ runicSmith   $ genDwarf r1
      (Just "greybeard")    -> rpg $ greyBeard    $ genDwarf r1
      (Just "deepwatch")    -> rpg $ deepWatch    $ genDwarf r1
      (Just "clanwarrior")  -> rpg $ clanWarrior  $ genDwarf r1
      (Just "hit")          -> tohit
      (Just "wound")        -> towound
      otherwise             -> printf "%d %s +/- %d %s = %d\n" (dieAmt ds) (dieType ds) (dieMod ds) (show r0) ((sum r0) + (dieMod ds))
