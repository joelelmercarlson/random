module Human (genHuman) where
  import DiceSet (d10, d20, twoD10)
  import Character
  import Util

  -- | human know thy self
  -- | require lots of d10
  genHuman :: IO Character
  genHuman = do
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
    age' <- d10
    gender' <- d10
    r0 <- d10
    r1 <- d10
    r2 <- twoD10
    r3 <- twoD10
    r4 <- twoD10
    r5 <- d20
    r6 <- d20
    return $ Character {
      weaponSkill = 20 + ws
      , ballisticSkill = 20 + bs
      , strength = 20 + s
      , toughness = 20 + t
      , initiative = 20 + i
      , agility = 20 + ag
      , dexterity = 20 + dex
      , intelligence = 20 + int'
      , willpower = 20 + wp
      , fellowship = 20 + fel
      , movement  = 4
      , fate = 2
      , resilience = 1
      , race   = "Human"
      , gender = genders gender'
      , age    = 15 + age'
      , place  = worlds 1 r0 r1 places places1 places2
      , eye    = eyes r2
      , hair   = hairs r3
      , height = 57 + r4
      , mark   = pick r5 marks
      , name   = names (genders gender') r6 female male
    }

  -- | data
  female :: [String]
  female = [ "Alexa"
           , "Alfrida"
           , "Betrix"
           , "Bianka"
           , "Carlott"
           , "Elfrida"
           , "Elise"
           , "Gabrielle"
           , "Gretchen"
           , "Hanna"
           , "Ilsa"
           , "Klara"
           , "Jarla"
           , "Ludmilla"
           , "Mathilde"
           , "Regina"
           , "Solveig"
           , "Theodora"
           , "Ulrike"
           , "Wertha"
           ]

  male :: [String]
  male = [ "Adelbert"
         , "Albrecht"
         , "Berthold"
         , "Dieter"
         , "Eckhardt"
         , "Felix"
         , "Gottfried"
         , "Gustav"
         , "Heinz"
         , "Johann"
         , "Konrad"
         , "Leopold"
         , "Magnus"
         , "Otto"
         , "Pieter"
         , "Rudiger"
         , "Seigfried"
         , "Ulrich"
         , "Waldemar"
         , "Wolfgang"
         ]

  eyes :: Int -> String
  eyes x = color
    where
      color
        | x == 2 = "Free Choice"
        | x == 3 = "Green"
        | x == 4 = "Pale Blue"
        | x >= 5 && x <= 7   = "Blue"
        | x >= 8 && x <= 11  = "Pale Grey"
        | x >= 12 && x <= 14 = "Grey"
        | x >= 15 && x <= 17 = "Brown"
        | x == 18 = "Hazel"
        | x == 19 = "Dark Brown"
        | x == 20 = "Black"
        | otherwise = "nil"

  hairs :: Int -> String
  hairs x = color
    where
      color
        | x == 2 = "White Blond"
        | x == 3 = "Golden Brown"
        | x == 4 = "Red Blond"
        | x >= 5 && x <= 7   = "Golden Brown"
        | x >= 8 && x <= 11  = "Light Brown"
        | x >= 12 && x <= 14 = "Dark Brown"
        | x >= 15 && x <= 17 = "Black"
        | x == 18 = "Auburn"
        | x == 19 = "Red"
        | x == 20 = "Grey"
        | otherwise = "nil"

  places :: [String]
  places = [ "Human" ]

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

  marks :: [String]
  marks = [ "Pox Marks"
          , "Ruddy Faced"
          , "Scar"
          , "Tattoo"
          , "Earring"
          , "Ragged Ear"
          , "Nose Ring"
          , "Wart"
          , "Broken Nose"
          , "Missing Tooth"
          , "Snaggle Teeth"
          , "Lazy Eye"
          , "Missing Eyebrow(s)"
          , "Missing Digit"
          , "Distinctive Gait"
          , "Curious Smell"
          , "Huge Nose"
          , "Large Mole"
          , "Small Bald Patch"
          , "Strange Coloured Eye(s)"
          ]
