module Hobbit (genHobbit) where
  import DiceSet (d10, d20, d50, d100, twoD10)
  import Character
  import Util

  -- | hobbit know thy self
  -- | require lots of d10
  genHobbit :: IO Character
  genHobbit = do
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
    age' <- d50
    gender' <- d10
    r0 <- d10
    r1 <- d10
    r2 <- d10
    r3 <- d10
    r4 <- d10
    r5 <- d10
    r6 <- d20
    r7 <- d20
    r8 <- d100
    return $ Character {
      weaponSkill = 10 + ws
      , ballisticSkill = 30 + bs
      , strength = 10 + s
      , toughness = 20 + t
      , initiative = 20 + i
      , agility = 20 + ag
      , dexterity = 30 + dex
      , intelligence = 20 + int'
      , willpower = 30 + wp
      , fellowship = 30 + fel
      , movement = 3
      , fate = 0
      , resilience = 2
      , race   = "Hobbit"
      , gender = genders gender'
      , age    = 15 + age'
      , place  = worlds (pickBirth r0) r1 r2 places places1 places2
      , eye    = eyes r3
      , hair   = hairs r4
      , height = 37 + r5
      , mark   = pick r6 marks
      , name   = names (genders gender') r7 female male
      , career = careers r8
    }

  -- | data
  careers :: Int -> String
  careers x = status
    where
      status
        | x <= 8 = "Academic"
        | x <= 33 = "Burgher"
        | x <= 46 = "Courtier"
        | x <= 57 = "Peasant"
        | x <= 68 = "Ranger"
        | x <= 82 = "Riverfolk"
        | x <= 94 = "Rogue"
        | x <= 100 = "Warrior"
        | otherwise = "nil"

  eyes :: Int -> String
  eyes x = color
    where
      color
        | x == 2 = "Light Grey"
        | x == 3 = "Grey"
        | x == 4 = "Pale Blue"
        | x >= 5 && x <= 7   = "Blue"
        | x >= 8 && x <= 11  = "Green"
        | x >= 12 && x <= 14 = "Hazel"
        | x >= 15 && x <= 17 = "Brown"
        | x == 18 = "Copper"
        | x == 19 = "Dark Brown"
        | x == 20 = "Dark Brown"
        | otherwise = "nil"

  female :: [String]
  female = [ "Cerasta Twofoot"
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

  hairs :: Int -> String
  hairs x = color
    where
      color
        | x == 2 = "Grey"
        | x == 3 = "Flazen"
        | x == 4 = "Russet"
        | x >= 5 && x <= 7   = "Honey"
        | x >= 8 && x <= 11  = "Chestnut"
        | x >= 12 && x <= 14 = "Ginger"
        | x >= 15 && x <= 17 = "Mustard"
        | x == 18 = "Almond"
        | x == 19 = "Chocolate"
        | x == 20 = "Liquirice"
        | otherwise = "nil"

  male :: [String]
  male = [ "Wilcome Gardner"
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

  pickBirth :: Int -> Int
  pickBirth n = if n < 5
                 then 1
                 else 2

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
