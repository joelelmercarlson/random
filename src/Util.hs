module Util ( pick
            , pn
            , nth
            , genders
            , heights
            , height_f
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

  pn :: Int -> [Int] -> Int
  pn m n = case nth m n of
             Nothing  -> 0
             (Just y) -> y

  -- | nth safe chooser
  nth :: Int -> [a] -> Maybe a
  nth _ []     = Nothing
  nth 1 (x:_)  = Just x
  nth n (x:xs) = nth (n - 1) xs

  genders :: [String]
  genders = [ "Female", "Male" ]

  heights :: String -> [Int] -> Int
  heights n m = case n of
               "Female" -> pn 1 m
               "Male"   -> pn 2 m
               _        -> 60

  height_f :: Int -> String
  height_f m = (show ht_f) ++ "\'" ++ (show ht_i) ++ "\""
    where
      ht_f = m `div` 12
      ht_i = m - (ht_f * 12)

  names :: String -> Int -> [String] -> [String] -> String
  names m n o p = case m of
                "Female" -> pick n o
                "Male"   -> pick n p
                _        -> "Nil"

  wounds :: Int -> [Int] -> Int
  wounds m n = if m >= 1 && m < 4
             then pn 1 n
             else if m >= 4 && m < 7
                  then pn 2 n
                  else if m >= 7 && m < 9
                       then pn 3 n
                       else pn 4 n

  fates :: Int -> [Int] -> Int
  fates m n = if m >= 1 && m < 5
             then pn 1 n
             else if m >= 5 && m < 8
                  then pn 2 n 
                  else pn 3 n 

  worlds :: Int -> Int -> Int -> [String] -> [String] -> [String] -> String
  worlds m n o p0 p1 p2 = case pick m p0 of
              "Human" -> "a " ++ (pick o p2) ++ " in " ++ (pick n p1)
              _       -> pick m p0

