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
        -- | provide all the rolls to genDwarf
        d = genDwarf [20,20,20,20,20,20,20,20,1,1,1,1,1,1,1,1,1,1,1,1]

    printf "%d%s +/- %d %s = %d\n" da dt dm (show rs) t
    putStrLn $ show d


