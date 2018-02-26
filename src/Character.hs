module Character( Character(..)
                ) where

  data Character = Character {
    d10_rolls_t :: [Int]
    , ws :: Int
    , bs :: Int
    , s :: Int
    , t :: Int
    , ag :: Int
    , int :: Int
    , wp :: Int
    , fel :: Int
    , a :: Int
    , w :: Int
    , m :: Int
    , fp :: Int
    , race :: String
    , gender :: String
    , age :: Int
    , place :: String
    , eye :: String
    , hair :: String
    , height :: Int
    , weight :: Int
    , mark :: String
    , name :: String
    , career :: String
  } deriving (Show)
