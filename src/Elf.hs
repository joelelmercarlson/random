module Elf (genElf) where
  import Control.Monad.Random
  import DiceSet
  import Character
  import Util

  -- | elf know thy self
  -- | require lots of d10
  genElf :: IO Character
  genElf = do
    let die = (1, 10)
        dice = (2, 10)
        d100 = (10, 10)
    ws <- evalRandIO (rollDice dice)
    bs <- evalRandIO (rollDice dice)
    s <- evalRandIO (rollDice dice)
    t <- evalRandIO (rollDice dice)
    i <- evalRandIO (rollDice dice)
    ag <- evalRandIO (rollDice dice)
    dex <- evalRandIO (rollDice dice)
    int' <- evalRandIO (rollDice dice)
    wp <- evalRandIO (rollDice dice)
    fel <- evalRandIO (rollDice dice)
    age' <- evalRandIO (rollDice d100)
    r0 <- evalRandIO (rollDice die)
    r1 <- evalRandIO (rollDice die)
    r2 <- evalRandIO (rollDice die)
    r3 <- evalRandIO (rollDice die)
    r4 <- evalRandIO (rollDice die)
    r5 <- evalRandIO (rollDice die)
    r6 <- evalRandIO (rollDice die)
    r7 <- evalRandIO (rollDice die)
    r8 <- evalRandIO (rollDice die)
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
      , race   = "Elf"
      , gender = genders age'
      , age    = 30 + age'
      , place  = worlds (pickBirth r0) r1 r2 places places1 places2
      , eye    = pick r3 eyes
      , hair   = pick r4 hairs
      , height = 71 + r5
      , weight = 75 + (r6 + r7) * 5
      , mark   = "nil"
      , name   = names (genders age') r8 female male
      , career = "basic"
    }

  pickBirth :: Int -> Int
  pickBirth n
    | n < 2 = 1
    | n < 4 = 2
    | n < 6 = 3
    | n < 8 = 4
    | otherwise = 5

  -- | data
  female :: [String]
  female = [ "Nil"
           , "Alane"
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

  male :: [String]
  male = [ "Nil"
         , "Aluthol"
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

  eyes :: [String]
  eyes = [ "Grey Blue"
         , "Blue"
         , "Green"
         , "Copper"
         , "Light Brown"
         , "Brown"
         , "Dark Brown"
         , "Silver"
         , "Purple"
         , "Black"
         ]

  hairs :: [String]
  hairs = [ "Silver"
          , "Ash Bond"
          , "Corn"
          , "Yellow"
          , "Copper"
          , "Light Brown"
          , "Light Brown"
          , "Brown"
          , "Dark Brown"
          , "Black"
          ]

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
