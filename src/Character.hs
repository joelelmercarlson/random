module Character (Character(..)) where
  data Character = Character {
    weaponSkill :: Int
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
    , resilience :: Int
    , race :: String
    , gender :: String
    , age :: Int
    , place :: String
    , eye :: String
    , hair :: String
    , height :: Int
    , mark :: String
    , name :: String
    , career :: String
  } deriving (Show)
