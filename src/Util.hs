module Util (pick
            , pn
            , nth
            , average
            , clamp
            , clampZ
            , genders
            , heightF
            , names
            , worlds
            , wounds) where

  -- | bonus
  bonus :: Int -> Int
  bonus n = clamp $ n `div` 10

  -- | pick
  pick :: Int -> [String] -> String
  pick n m = case nth n m of
              Nothing  -> "Nil"
              (Just y) -> y

  -- | remove Maybe from nth
  pn :: Int -> [Int] -> Int
  pn m n = case nth m n of
             Nothing  -> 1
             (Just y) -> y

  -- | nth safe chooser
  nth :: Int -> [a] -> Maybe a
  nth _ []     = Nothing
  nth 1 (x:_)  = Just x
  nth n (x:xs) = nth (n - 1) xs

  average :: [Int] -> Float
  average [] = 1
  average n = fromIntegral (sum n) / fromIntegral (length n)

  clamp :: Int -> Int
  clamp n = if n > 10 then 10 else clampZ n

  clampZ :: Int -> Int
  clampZ n = if n > 1 then n else 1

  genders :: Int -> String
  genders n = if n < 5 || n < 50 then "Female" else "Male"

  heightF :: Int -> String
  heightF m = show ht_f ++ "\'" ++ show ht_i ++ "\""
    where
      ht_f = m `div` 12
      ht_i = m - (ht_f * 12)

  names :: String -> Int -> [String] -> [String] -> String
  names m n female male = case m of
    "Female" -> pick n female
    "Male"   -> pick n male
    _        -> "Nil"

  worlds :: Int -> Int -> Int -> [String] -> [String] -> [String] -> String
  worlds m n o racial human1 human2 = case pick m racial of
    "Human" -> pick o human2 ++ " in " ++ pick n human1
    _       -> pick m racial

  wounds :: String -> Int -> Int -> Int -> Int
  wounds n sb tb wp = case n of
    "Hobbit" -> (2 * bonus tb) + (bonus wp)
    _ -> (bonus sb) + (2 * bonus tb) + (bonus wp)
