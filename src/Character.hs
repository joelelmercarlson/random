module Character(Character(..)) where

  data Character = Character {
    d10_rolls_t :: [Int]
    , weaponSkill :: Int
    , ballisticSkill :: Int
    , strength :: Int
    , toughness :: Int
    , initiative :: Int
    , agility :: Int
    , dexterity :: Int
    , intelligence :: Int
    , willpower :: Int
    , fellowship :: Int
    , movement :: Int
    , fate :: Int
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
