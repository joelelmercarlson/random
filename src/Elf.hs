module Elf (genElf) where
  import DiceSet (d10, d20, d100, twoD10)
  import Character
  import Util

  -- | elf know thy self
  -- | require lots of d10
  genElf :: IO Character
  genElf = do
    ws <- twoD10
    bs <- twoD10
    s <- twoD10
    t <- twoD10
    i <- twoD10
    ag <- twoD10
    dex <- twoD10
    int' <- twoD10
    wp <- twoD10
    fel <- twoD10
    age' <- d100
    gender' <- d10
    r0 <- d10
    r1 <- twoD10
    r2 <- twoD10
    r3 <- d10
    r4 <- d20
    r5 <- d100
    return $ Character {
      weaponSkill = 30 + ws
      , ballisticSkill = 30 + bs
      , strength = 20 + s
      , toughness = 20 + t
      , initiative = 40 + i
      , agility = 30 + ag
      , dexterity = 30 + dex
      , intelligence = 30 + int'
      , willpower = 30 + wp
      , fellowship = 20 + fel
      , movement  = 5
      , fate = 0
      , resilience = 0
      , race   = elves r0
      , gender = genders gender'
      , age    = 30 + age'
      , place  = pick (pickBirth r0) places
      , eye    = pickEyes r0 r1
      , hair   = pickHairs r0 r2
      , height = 71 + r3
      , mark   = "nil"
      , name   = names (genders gender') r4 female male
      , career = pickCareers r0 r5
    }

  -- | data
  careers :: Int -> String
  careers x = status
    where
      status
        | x <= 16 = "Academic"
        | x <= 29 = "Burgher"
        | x <= 45 = "Courtier"
        | x <= 56 = "Peasant"
        | x <= 63 = "Ranger"
        | x <= 80 = "Riverfolk"
        | x <= 88 = "Rogue"
        | x <= 100 = "Warrior"
        | otherwise = "nil"

  careers' :: Int -> String
  careers' x = status
    where
      status
        | x <= 5 = "Academic"
        | x <= 10 = "Burgher"
        | x <= 35 = "Courtier"
        | x <= 68 = "Peasant"
        | x <= 78 = "Ranger"
        | x == 79 = "Riverfolk"
        | x <= 90 = "Rogue"
        | x <= 100 = "Warrior"
        | otherwise = "nil"

  eyes :: Int -> String
  eyes x = color
    where
      color
        | x == 2 = "Jet"
        | x == 3 = "Amethyst"
        | x == 4 = "Aquamarine"
        | x >= 5 && x <= 7   = "Sapphire"
        | x >= 8 && x <= 11  = "Turquoise"
        | x >= 12 && x <= 14 = "Emerald"
        | x >= 15 && x <= 17 = "Amber"
        | x == 18 = "Copper"
        | x == 19 = "Citrine"
        | x == 20 = "Gold"
        | otherwise = "nil"

  eyes' :: Int -> String
  eyes' x = color
    where
      color
        | x == 2 = "Ivory"
        | x == 3 = "Charcoal"
        | x == 4 = "Ivy Green"
        | x >= 5 && x <= 7   = "Mossy Green"
        | x >= 8 && x <= 11  = "Chestnut"
        | x >= 12 && x <= 14 = "Chestnut"
        | x >= 15 && x <= 17 = "Dark Brown"
        | x == 18 = "Tan"
        | x == 19 = "Sandy Brown"
        | x == 20 = "Violet"
        | otherwise = "nil"

  elves :: Int -> String
  elves x = elf'
    where
      elf'
        | x < 6 = "High Elf"
        | otherwise = "Wood Elf"

  female :: [String]
  female = [ "Alane"
           , "Altronia"
           , "Davandrel"
           , "Eldril"
           , "Eponia"
           , "Fanriel"
           , "Filamir"
           , "Gallina"
           , "Halion"
           , "Illudil"
           , "Ionor"
           , "Lindara"
           , "Lorandara"
           , "Maruviel"
           , "Pelgrana"
           , "Siluvaine"
           , "Tallana"
           , "Ulliana"
           , "Vivandrel"
           , "Yuviel"
           ]

  hairs :: Int -> String
  hairs x = color
    where
      color
        | x == 2 = "Silver"
        | x == 3 = "White"
        | x == 4 = "Pale Blond"
        | x >= 5 && x <= 7   = "Blond"
        | x >= 8 && x <= 11  = "Yellow Blond"
        | x >= 12 && x <= 14 = "Copper Blond"
        | x >= 15 && x <= 17 = "Red Blond"
        | x == 18 = "Auburn"
        | x == 19 = "Red"
        | x == 20 = "Black"
        | otherwise = "nil"

  hairs' :: Int -> String
  hairs' x = color
    where
      color
        | x == 2 = "Birch Silver"
        | x == 3 = "Ash Blond"
        | x == 4 = "Rose Gold"
        | x >= 5 && x <= 7   = "Honey Blond"
        | x >= 8 && x <= 11  = "Brown"
        | x >= 12 && x <= 14 = "Mahogany Brown"
        | x >= 15 && x <= 17 = "Dark Brown"
        | x == 18 = "Sienna"
        | x == 19 = "Ebony"
        | x == 20 = "Blue-Black"
        | otherwise = "nil"

  male :: [String]
  male = [ "Aluthol"
         , "Aamendil"
         , "Angran"
         , "Cavindel"
         , "Dolwen"
         , "Eldillor"
         , "Falandar"
         , "Farnoth"
         , "Gildiril"
         , "Harrond"
         , "Imhol"
         , "Larandar"
         , "Laurenor"
         , "Mellion"
         , "Mormacar"
         , "Ravandil"
         , "Torendil"
         , "Urdithane"
         , "Valahuir"
         , "Yavandir"
         ]

  pickBirth :: Int -> Int
  pickBirth x = birth
    where
      birth
        | x < 4 = 1
        | x < 6 = 2
        | x == 7 = 3
        | x == 8 = 4
        | otherwise = 5

  pickCareers :: Int -> Int -> String
  pickCareers x y = status
    where
      status
        | x < 6 = careers y
        | otherwise = careers' y

  pickEyes :: Int -> Int -> String
  pickEyes x y = color
    where
      color
        | x < 6 = eyes y
        | otherwise = eyes' y

  pickHairs :: Int -> Int -> String
  pickHairs x y = color
    where
      color
        | x < 6 = hairs y
        | otherwise = hairs' y

  places :: [String]
  places = [ "City of Altdorf"
           , "City of Marienburg"
           , "Laurelorn Forest"
           , "The Great Forest"
           , "Reikwald Forest"
           ]
