module Main where
  import System.Environment
  import Text.Printf

  import DiceSet
  import Dwarf

  main :: IO ()
  main = do
    xs <- getArgs
    let ds = if length xs > 2 then modRoll xs else zModRoll xs

    rs <- roll ds
    let t  = (sum rs) + dm
        dt = dieType ds
        da = dieAmt  ds
        dm = dieMod  ds
        tens = zModRoll ["30", "d10"]
        twenties = zModRoll ["10", "d20"]

    printf "%d%s +/- %d %s = %d\n" da dt dm (show rs) t

    rs1 <- roll tens
    rs2 <- roll twenties

    putStrLn $ show $ genDwarf rs1 rs2


