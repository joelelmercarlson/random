module Character( Actor(..)
                ) where

  data Actor = Actor {
    ws   :: Int
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
    , ip :: Int
    , fp :: Int
    , race :: String
    , gender :: String
    , archtype :: String
    , place :: String
    , age :: Int
    , eye :: Int
    , weight :: Int
    , hair :: Int
    , height :: Int
    , mark :: String
    , ws_r :: Int
    , bs_r :: Int
    , s_r :: Int
    , t_r :: Int
    , ag_r :: Int
    , int_r :: Int
    , wp_r :: Int
  } deriving (Show)
