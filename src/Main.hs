module Main where
  import System.Environment
  import Text.Printf

  import Character
  import DiceSet
  import Util (nth, height_f)
  import Dwarf
  import Elf
  import Halfling 
  import Human

  main :: IO ()
  main = do
    xs <- getArgs

    let x  = zModRoll ["20", "d10"]
        y  = zModRoll ["20", "d20"]
        ds = if length xs > 2 then modRoll xs else zModRoll xs

    r0 <- roll ds
    r1 <- roll x
    r2 <- roll y

    case nth 1 xs of
      (Just "dwarf")  -> display $ genDwarf    r1 r2
      (Just "elf")    -> display $ genElf      r1 r2
      (Just "hobbit") -> display $ genHalfling r1 r2
      (Just "human")  -> display $ genHuman    r1 r2
      otherwise       -> printf "%d %s +/- %d %s = %d\n" (dieAmt ds) (dieType ds) (dieMod ds) (show r0) ((sum r0) + (dieMod ds))

  display :: Character -> IO ()
  display n = do
    printf "%s, the %s from %s\n\n" (name n) (race n) (place n)
    printf "| Age: %-19d | Gender: %-7s |\n" (age n) (gender n)
    printf "| Eye  Color: %-12s | Weight: %3d lbs |\n" (eye n) (weight n)
    printf "| Hair Color: %-12s | Height: %-7s |\n" (hair n) (height_f (height n))
    printf "| Distinguishing Mark: %-21s |\n\n" (mark n)
    printf "Main Profile\n"
    printf "   | WS | BS | S  | T  | Ag | Int | Wp | Fel |\n"
    printf "   | %-2d | %-2d | %-2d | %-2d | %-2d | %-3d | %-2d | %-3d |\n" (ws n) (bs n) (s n) (t n) (ag n) (int n) (wp n) (fel n)
    printf "Secondary Profile\n"
    printf "   | A  | W  | SB | TB | M  | Mag | IP | FP  |\n"
    printf "   | %-2d | %-2d | %-2d | %-2d | %-2d | %-3d | %-2d | %-3d |\n" (a n) (w n) (sb n) (tb n) (m n) (mag n) (ip n) (fp n)
