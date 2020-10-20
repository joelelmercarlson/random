module Hobbit(genHobbit) where
  import Character
  import Util

  -- | hobbit know thy self
  -- | require lots of d10
  genHobbit :: [Int] -> Character
  genHobbit m = do
    Character {
      d10_rolls_t = m
      , weaponSkill = 10 + pn 1 m + pn 2 m
      , ballisticSkill = 30 + pn 3 m + pn 4 m
      , strength = 10 + pn 5 m + pn 6 m
      , toughness = 20 + pn 7 m + pn 8 m
      , initiative = 20 + pn 9 m + pn 10 m
      , agility = 20 + pn 11 m + pn 12 m
      , dexterity = 30 + pn 13 m + pn 14 m
      , intelligence = 20 + pn 15 m + pn 16 m
      , willpower = 30 + pn 17 m + pn 18 m
      , fellowship = 30 + pn 19 m + pn 20 m
      , movement = 3
      , fate = 0
      , race   = "Hobbit"
      , gender = genders (average m)
      , age    = 15 + sum (take 5 m)
      , place  = worlds (pickBirth (pn 21 m)) (pn 22 m) (pn 23 m) places places1 places2
      , eye    = pick (pn 24 m) eyes
      , hair   = pick (pn 25 m) hairs
      , height = 37 + (pn 1 m)
      , weight = 75 + (pn 26 m + pn 27 m) * 3
      , mark   = pick (pn 28 m + pn 29 m) marks
      , name   = names (genders (average m)) (pn 30 m + pn 31 m) female male
      , career = "basic"
    }

  pickBirth :: Int -> Int
  pickBirth n = if n < 5
                 then 1
                 else 2
  -- | data
  female :: [String]
  female = [ "Nil"
           , "Cerasta Twofoot"
           , "Mahonia Fallohide"
           , "Amaryllis Townsend"
           , "Calaminth Rumble"
           , "Salva Gamwich"
           , "Sapphire Boffin"
           , "Malva Goodchild"
           , "Kalmia Longhole"
           , "Clematis Brown"
           , "Forsythia Proudfoot"
           , "Belba Greenholm"
           , "Poinsetta Gammidge"
           , "Dianthus Brownlock"
           , "Peona Labingi"
           , "Opal Boffin"
           , "Salva Meadows"
           , "Hanna Harfoot"
           , "Salvia Marsh"
           , "Peony Bunce"
           , "Cleoma Hornblower"
           ]


  male :: [String]
  male = [ "Nil"
         , "Wilcome Gardner"
         , "Brocard Diggle"
         , "Haiduc Brown"
         , "Britius Burrow"
         , "Reginar Burrows"
         , "Hartgard Sandheaver"
         , "Reolus Harfoot"
         , "Porro Twofoot"
         , "Brice Gamgee"
         , "Pepin Goldworthy"
         , "Medard Burrow"
         , "Anno Banks"
         , "Turpin Burrows"
         , "Munderic Brockhouse"
         , "Dreux Stoor"
         , "Folmar Chubb"
         , "Orderic Stoor"
         , "Fortinbras Smallburrow"
         , "Jago Sandheaver"
         , "Isengar Brownlock"
         ]

  eyes :: [String]
  eyes = [ "Blue"
         , "Hazel"
         , "Hazel"
         , "Light Brown"
         , "Light Brown"
         , "Brown"
         , "Brown"
         , "Dark Brown"
         , "Dark Brown"
         , "Dark Brown"
         ]

  hairs :: [String]
  hairs = [ "Ash Blond"
          , "Corn"
          , "Yellow"
          , "Yellow"
          , "Copper"
          , "Red"
          , "Light Brown"
          , "Brown"
          , "Dark Brown"
          , "Black"
          ]

  places :: [String]
  places = [ "The Moot"
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
