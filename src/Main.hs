module Main where
  import System.Environment
  import Text.Printf

  import DiceSet
  import Util (nth)
  import Dwarf
  import Elf
  import Human

  main :: IO ()
  main = do
    xs <- getArgs

    let x = zModRoll ["20", "d10"]
        y = zModRoll ["20", "d20"]
        ds = if length xs > 2 then modRoll xs else zModRoll xs

    r0 <- roll ds
    r1 <- roll x
    r2 <- roll y

    case nth 1 xs of
      (Just "dwarf") -> putStrLn $ show $ genDwarf r1 r2
      (Just "elf")   -> putStrLn $ show $ genElf r1 r2
      (Just "human") -> putStrLn $ show $ genHuman r1 r2
      otherwise      -> printf "%d %s +/- %d %s = %d\n" (dieAmt ds) (dieType ds) (dieMod ds) (show r0) ((sum r0) + (dieMod ds))

