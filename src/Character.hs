module Character( Character(..)
                ) where

  data Character = Character {
    rolls_t :: [Int]
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
    , sb :: Int
    , tb :: Int
    , m :: Int
    , mag :: Int
    , ip :: Int
    , fp :: Int
    , race :: String
    , gender :: String
    , archtype :: String
    , place :: String
    , age :: Int
    , eye :: String
    , weight :: Int
    , hair :: String
    , height :: Int
    , heightft :: String
    , mark :: String
    , name :: String
    , wounds_t :: [Int]
    , fates_t :: [Int]
    , heights_t :: [Int]
  } deriving (Show)
