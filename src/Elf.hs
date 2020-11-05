module Elf (genElf) where
  import DiceSet (d10, d100, twoD10)
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
    r1 <- d10
    r2 <- d10
    r3 <- d10
    r4 <- d10
    r5 <- d10
    r6 <- d10
    r7 <- d100
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
      , race   = "High Elf"
      , gender = genders gender'
      , age    = 30 + age'
      , place  = worlds (pickBirth r0) r1 r2 places places1 places2
      , eye    = eyes r3
      , hair   = hairs r4
      , height = 71 + r5
      , mark   = "nil"
      , name   = names (genders gender') r6 female male
      , career = careers r7
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
  pickBirth n = birth
    where
      birth
        | n < 2 = 1
        | n < 4 = 2
        | n < 6 = 3
        | n < 8 = 4
        | otherwise = 5

  places :: [String]
  places = [ "City of Altdorf"
           , "City of Marienburg"
           , "Laurelorn Forest"
           , "The Great Forest"
           , "Reikwald Forest"
           ]

  places1 :: [String]
  places1 = [ "Averland"
            , "Hochland"
            , "Middenland"
            , "Nordland"
            , "Ostermark"
            , "Ostland"
            , "Reikland"
            , "Stirland"
            , "Talabecland"
            , "Wissenland"
            ]

  places2 :: [String]
  places2 = [ "City"
            , "Prosperous Town"
            , "Market Town"
            , "Fortified Town"
            , "Farming Village"
            , "Poor Village"
            , "Small Settlement"
            , "Pig/Cattle Farm"
            , "Arable Farm"
            , "Hovel"
            ]
