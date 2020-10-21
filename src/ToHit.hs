module ToHit (tohit, towound) where
  import Text.Printf

  hitT :: [[String]]
  hitT = [
           ["WS", "A:1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]
         , ["D:1",  "4", "3", "3", "3", "3", "3", "3", "3", "3", "3"]
         , ["2",  "4", "4", "3", "3", "3", "3", "3", "3", "3", "3"]
         , ["3",  "5", "4", "4", "3", "3", "3", "3", "3", "3", "3"]
         , ["4",  "5", "4", "4", "4", "3", "3", "3", "3", "3", "3"]
         , ["5",  "5", "5", "4", "4", "4", "3", "3", "3", "3", "3"]
         , ["6",  "5", "5", "4", "4", "4", "4", "3", "3", "3", "3"]
         , ["7",  "5", "5", "5", "4", "4", "4", "4", "3", "3", "3"]
         , ["8",  "5", "5", "5", "4", "4", "4", "4", "4", "3", "3"]
         , ["9",  "5", "5", "5", "5", "4", "4", "4", "4", "4", "3"]
         , ["10", "5", "5", "5", "5", "4", "4", "4", "4", "4", "4"]
         ]

  woundT :: [[String]]
  woundT = [
           ["  ",  "S:1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]
         , ["T:1", "4", "3", "2", "2", "2", "2", "2", "2", "2", "2"]
         , ["2",  "5", "4", "3", "2", "2", "2", "2", "2", "2", "2"]
         , ["3",  "6", "5", "4", "3", "2", "2", "2", "2", "2", "2"]
         , ["4",  "6", "6", "5", "4", "3", "2", "2", "2", "2", "2"]
         , ["5",  "6", "6", "6", "5", "4", "3", "2", "2", "2", "2"]
         , ["6",  "6", "6", "6", "6", "5", "4", "3", "2", "2", "2"]
         , ["7",  "6", "6", "6", "6", "6", "5", "4", "3", "2", "2"]
         , ["8",  "6", "6", "6", "6", "6", "6", "5", "4", "3", "2"]
         , ["9",  "6", "6", "6", "6", "6", "6", "6", "5", "4", "3"]
         , ["10", "6", "6", "6", "6", "6", "6", "6", "6", "5", "4"]
         ]

  banner :: String -> IO ()
  banner = printf "\x1b[32;1m%s\x1b[0m\n"

  tohit :: IO ()
  tohit = do
    banner "The Close Combat To-Hit Table"
    display hitT
    banner "WS - Weapon Skill | A - Attacker | D - Defender"

  towound :: IO ()
  towound = do
    banner "To-Wound Table"
    display' woundT
    banner "S - Strength | T - Toughness"

  fmtXY :: String -> IO ()
  fmtXY = printf "\x1b[47;1m%4s\x1b[0m"

  elemXY :: String -> IO ()
  elemXY  n = case n of
      "2" -> fmtXY n
      "3" -> fmtXY n
      "5" -> fmtXY n
      "6" -> fmtXY n
      "7" -> fmtXY n
      "8" -> fmtXY n
      "9" -> fmtXY n
      "10"  -> fmtXY n
      _ -> printf "\x1b[32;1m%4s\x1b[0m" n

  rowxy :: [String] -> IO ()
  rowxy [] = printf "\n"
  rowxy (x:xs) = do
    elemXY x
    rowxy xs

  display :: [[String]] -> IO ()
  display [] = printf "\n"
  display (x:xs) = do
    rowxy x
    display xs

  elemXY' :: String -> IO ()
  elemXY'  n = case n of
      "2" -> fmtXY n
      "6" -> fmtXY n
      "7" -> fmtXY n
      "8" -> fmtXY n
      "9" -> fmtXY n
      "10" -> fmtXY n
      _ -> printf "\x1b[32;1m%4s\x1b[0m" n

  rowxy' :: [String] -> IO ()
  rowxy' [] = printf "\n"
  rowxy' (x:xs) = do
    elemXY' x
    rowxy' xs

  display' :: [[String]] -> IO ()
  display' [] = printf "\n"
  display' (x:xs) = do
    rowxy' x
    display' xs
