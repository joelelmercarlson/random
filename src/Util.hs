module Util where

  -- | average
  average :: [Int] -> Float
  average [] = 1
  average n = fromIntegral (sum n) / fromIntegral (length n)

  -- | bonus
  bonus :: Int -> Int
  bonus n = clamp $ n `div` 10

  -- | clamp
  clamp :: Int -> Int
  clamp n = if n > 10 then 10 else clampZ n

  clampZ :: Int -> Int
  clampZ n = if n > 1 then n else 1

  -- | adventure genders
  genders :: Int -> String
  genders n = if n < 4 || n < 40 then "Female" else "Male"

  -- | height calculator
  heightF :: Int -> String
  heightF x = show ht_f ++ "\'" ++ show ht_i ++ "\""
    where
      ht_f = x `div` 12
      ht_i = x - (ht_f * 12)

  -- name picker
  names :: String -> Int -> [String] -> [String] -> String
  names m n female male = case m of
    "Female" -> pick n female
    "Male"   -> pick n male
    _        -> "Nil"

  -- | nth safe chooser
  nth :: Int -> [a] -> Maybe a
  nth _ []     = Nothing
  nth 1 (x:_)  = Just x
  nth n (x:xs) = nth (n - 1) xs

  -- | pick
  pick :: Int -> [String] -> String
  pick x xs = case nth x xs of
    Nothing  -> "Nil"
    (Just y) -> y

  -- | where in the world
  worlds :: Int -> Int -> Int -> [String] -> [String] -> [String] -> String
  worlds m n o racial human1 human2 = case pick m racial of
    "Human" -> pick o human2 ++ " in " ++ pick n human1
    _       -> pick m racial

  -- | hit points
  wounds :: String -> Int -> Int -> Int -> Int
  wounds n sb tb wp = case n of
    "Hobbit" -> (2 * bonus tb) + (bonus wp)
    _ -> (bonus sb) + (2 * bonus tb) + (bonus wp)
