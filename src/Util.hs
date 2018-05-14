module Util ( pick
            , pn
            , nth
            , average
            , clamp
            , clampZ
            , genders
            , heights
            , heightF
            , names
            , wounds
            , fates
            , worlds
            ) where

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

  genders :: Float -> String
  genders n = if n < 5.1 then "Female" else "Male"

  heights :: String -> [Int] -> Int
  heights n m = case n of
               "Female" -> pn 1 m
               "Male"   -> pn 2 m
               _        -> 60

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

  wounds :: Int -> [Int] -> Int
  wounds m n
   | m >= 1 && m < 4 = pn 1 n
   | m >= 4 && m < 7 = pn 2 n
   | m >= 7 && m < 9 = pn 3 n
   | otherwise = pn 4 n

  fates :: Int -> [Int] -> Int
  fates m n
    | m >= 1 && m < 5 = pn 1 n
    | m >= 5 && m < 8 = pn 2 n
    | otherwise = pn 3 n

  worlds :: Int -> Int -> Int -> [String] -> [String] -> [String] -> String
  worlds m n o racial human1 human2 = case pick m racial of
              "Human" -> pick o human2 ++ " in " ++ pick n human1
              _       -> pick m racial
