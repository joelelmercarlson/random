module Util (
            ) where
  -- | pick
  pick :: Int -> [String] -> String
  pick n m = case nth n m of
              Nothing  -> "Nil"
              (Just y) -> y

  -- | nth safe chooser
  nth :: Int -> [a] -> Maybe a
  nth _ []     = Nothing
  nth 1 (x:_)  = Just x
  nth n (x:xs) = nth (n - 1) xs

  genders :: [String]
  genders = [ "Female", "Male" ]

  heights :: String -> Int
  heights n = case n of
               "Female" -> 64
               "Male"   -> 66
               _        -> 62

  names :: String -> Int -> String
  names m n = case m of
                "Female" -> (pick n female)
                "Male"   -> (pick n male)
                _        -> "Nil"

