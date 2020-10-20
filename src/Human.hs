module Human(genHuman) where
  import Character
  import Util

  -- | human know thy self
  -- | require lots of d10
  genHuman :: [Int] -> Character
  genHuman m = do
    Character {
      d10_rolls_t = m
      , weaponSkill = 20 + pn 1 m + pn 2 m
      , ballisticSkill = 20 + pn 3 m + pn 4 m
      , strength = 20 + pn 5 m + pn 6 m
      , toughness = 20 + pn 7 m + pn 8 m
      , initiative = 20 + pn 9 m + pn 10 m
      , agility = 20 + pn 11 m + pn 12 m
      , dexterity = 20 + pn 13 m + pn 14 m
      , intelligence = 20 + pn 15 m + pn 16 m
      , willpower = 20 + pn 17 m + pn 18 m
      , fellowship = 20 + pn 19 m + pn 20 m
      , movement  = 4
      , fate = 2
      , race   = "Human"
      , gender = genders (average m)
      , age    = 15 + (pn 1 m)
      , place  = worlds 1 (pn 21 m) (pn 22 m) places places1 places2
      , eye    = pick (pn 23 m) eyes
      , hair   = pick (pn 24 m) hairs
      , height = 57 + pn 1 m + pn 2 m
      , weight = 100 + (pn 25 m + pn 26 m) * 5
      , mark   = pick (pn 27 m + pn 28 m) marks
      , name   = names (genders (average m)) (pn 29 m + pn 30 m) female male
      , career = "basic"
    }

  -- | data
  female :: [String]
  female = [ "Nil"
           , "Alexa"
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
  male = [ "Nil"
         , "Adelbert"
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

  eyes :: [String]
  eyes = [ "Pale Grey"
         , "Grey Blue"
         , "Blue"
         , "Green"
         , "Copper"
         , "Light Brown"
         , "Brown"
         , "Dark Brown"
         , "Purple"
         , "Black"
         ]

  hairs :: [String]
  hairs = [ "Ash Blond"
          , "Corn"
          , "Yellow"
          , "Copper"
          , "Red"
          , "Light Brown"
          , "Brown"
          , "Brown"
          , "Dark Brown"
          , "Black"
          ]

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
  marks = [ "Nil"
          , "Pox Marks"
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
