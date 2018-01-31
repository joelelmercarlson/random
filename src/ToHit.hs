module ToHit ( tohit
             , towound
             ) where

  import Text.Printf

  hit_t :: [[String]]
  hit_t = [
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

  wound_t :: [[String]]
  wound_t = [
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
  banner n = do
    printf "\x1b[32;1m%s\x1b[0m\n" n

  tohit :: IO ()
  tohit = do
    banner "The Close Combat To-Hit Table"
    display hit_t
    banner "WS - Weapon Skill | A - Attacker | D - Defender"

  towound :: IO ()
  towound = do
    banner "To-Wound Table"
    display' wound_t
    banner "S - Strength | T - Toughness"

  fmt_xy :: String -> IO ()
  fmt_xy n = do
    printf "\x1b[47;1m%4s\x1b[0m" n

  elem_xy :: String -> IO ()
  elem_xy  n = do
    case n of
      "2" -> fmt_xy n
      "3" -> fmt_xy n
      "5" -> fmt_xy n
      "6" -> fmt_xy n
      "7" -> fmt_xy n
      "8" -> fmt_xy n
      "9" -> fmt_xy n
      "10"  -> fmt_xy n
      otherwise -> printf "\x1b[32;1m%4s\x1b[0m" n

  rowxy :: [String] -> IO ()
  rowxy [] = do
    printf "\n"
  rowxy (x:xs) = do
    elem_xy x
    rowxy xs

  display :: [[String]] -> IO ()
  display [] = do
    printf "\n"
  display (x:xs) = do
    rowxy x
    display xs

  elem_xy' :: String -> IO ()
  elem_xy'  n = do
    case n of
      "2" -> fmt_xy n
      "6" -> fmt_xy n
      "7" -> fmt_xy n
      "8" -> fmt_xy n
      "9" -> fmt_xy n
      "10" -> fmt_xy n
      otherwise -> printf "\x1b[32;1m%4s\x1b[0m" n

  rowxy' :: [String] -> IO ()
  rowxy' [] = do
    printf "\n"
  rowxy' (x:xs) = do
    elem_xy' x
    rowxy' xs

  display' :: [[String]] -> IO ()
  display' [] = do
    printf "\n"
  display' (x:xs) = do
    rowxy' x
    display' xs
