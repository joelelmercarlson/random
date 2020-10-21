module Main where
  import System.Environment
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
       _ -> do {tohit; towound; party}

  party :: IO ()
  party = do
    e <- genElf
    d <- genDwarf
    h <- genHuman
    r <- genHobbit
    putStrLn "A Party of Four Adventurers..."
    putStrLn "==============================\n"
    putStrLn $ "result: " ++ show e
    putStrLn $ "result: " ++ show d
    putStrLn $ "result: " ++ show h
    putStrLn $ "result: " ++ show r
    rpg $ e
    rpg $ d
    rpg $ h
    rpg $ r
