module Dwarf(genDwarf) where
  import Character
  import Util

  -- | dwarf know thy self
  -- | require lots of d10
  genDwarf :: [Int] -> Character
  genDwarf m = do
    Character {
      d10_rolls_t = m
      , weaponSkill = 30 + pn 1 m + pn 2 m
      , ballisticSkill = 20 + pn 3 m + pn 4 m
      , strength = 20 + pn 5 m + pn 6 m
      , toughness = 30 + pn 7 m + pn 8 m
      , initiative = 20 + pn 9 m + pn 10 m
      , agility = 10 + pn 11 m + pn 12 m
      , dexterity = 30 + pn 13 m + pn 14 m
      , intelligence = 20 + pn 15 m + pn 16 m
      , willpower = 40 + pn 17 m + pn 18 m
      , fellowship = 10 + pn 19 m + pn 20 m
      , movement  = 3
      , fate = 0
      , race   = "Dwarf"
      , gender = genders (average m)
      , age    = 15 + sum (take 10 m)
      , place  = worlds (pn 21 m) (pn 22 m) (pn 23 m) places places1 places2
      , eye    = pick (pn 24 m) eyes
      , hair   = pick (pn 25 m) hairs
      , height = 51 + pn 1 m
      , weight = 90 + (pn 26 m + pn 27 m) * 5
      , mark   = pick (pn 28 m + pn 29 m) marks
      , name   = names (genders (average m)) (pn 30 m + pn 31 m) female male
      , career = "basic"
    }

  -- | Data Tables
  female :: [String]
  female = [ "Nil"
           , "Anika"
           , "Asta"
           , "Astrid"
           , "Berta"
           , "Brigit"
           , "Dagmar"
           , "Elsa"
           , "Erika"
           , "Fanziska"
           , "Greta"
           , "Hunni"
           , "Ingrid"
           , "Janna"
           , "Karin"
           , "Petra"
           , "Sigrid"
           , "Sigrun"
           , "Silma"
           , "Thylda"
           , "Ulla"
           ]

  male :: [String]
  male = [ "Nil"
         , "Bardin"
         , "Brokk"
         , "Dimzad"
         , "Durak"
         , "Garil"
         , "Gottri"
         , "Grundi"
         , "Hargin"
         , "Imrak"
         , "Kargun"
         , "Jorunn"
         , "Magnar"
         , "Modrin"
         , "Nargond"
         , "Ozad"
         , "Ragnar"
         , "Snorri"
         , "Storri"
         , "Thingrim"
         , "Urgrim"
         ]

  eyes :: [String]
  eyes = [ "Pale Grey"
         , "Blue"
         , "Copper"
         , "Light Brown"
         , "Light Brown"
         , "Brown"
         , "Brown"
         , "Dark Brown"
         , "Dark Brown"
         , "Purple"
         ]

  hairs :: [String]
  hairs = [ "Ash Blond"
          , "Yellow"
          , "Red"
          , "Copper"
          , "Light Brown"
          , "Brown"
          , "Brown"
          , "Dark Brown"
          , "Blue Black"
          , "Black"
          ]

  places :: [String]
  places = [ "Karak Norn (Grey Mountains)"
           , "Karak Izor (the Vaults)"
           , "Karak Hirm (Black Mountains)"
           , "Karak Kadrin (World's Edge Mountains)"
           , "Karaz-A-Karak (World's Edge Mountains)"
           , "Zhufbar (World's Edge Mountains)"
           , "Baraz Varr (the Black Gulf)"
           , "Human"
           , "Human"
           , "Human"
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
