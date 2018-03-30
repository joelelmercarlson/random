module Main where
  import System.Environment

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

    let x = zModRoll ["100", "d10"]

    r <- roll x

    case nth 1 xs of
      Just "dwarf"        -> rpg $ genDwarf  r
      Just "elf"          -> rpg $ genElf    r
      Just "hobbit"       -> rpg $ genHobbit r
      Just "human"        -> rpg $ genHuman  r
      Just "king"         -> rpg $ king         $ genDwarf r
      Just "thane"        -> rpg $ thane        $ genDwarf r
      Just "dragonseeker" -> rpg $ dragonSeeker $ genDwarf r
      Just "runicsmith"   -> rpg $ runicSmith   $ genDwarf r
      Just "greybeard"    -> rpg $ greyBeard    $ genDwarf r
      Just "deepwatch"    -> rpg $ deepWatch    $ genDwarf r
      Just "clanwarrior"  -> rpg $ clanWarrior  $ genDwarf r
      Just "hit"          -> tohit
      Just "wound"        -> towound
      otherwise           -> do { tohit; towound; rpg $ genDwarf r }
