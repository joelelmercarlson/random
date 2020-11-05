module Dwarf (genDwarf) where
  import DiceSet (d10, d20, d100, twoD10)
  import Character
  import Util

  -- | dwarf know thy self
  -- | require lots of d10
  genDwarf :: IO Character
  genDwarf = do
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
    r6 <- d20
    r7 <- d20
    r8 <- d100
    return $ Character {
      weaponSkill = 30 + ws
      , ballisticSkill = 20 + bs
      , strength = 20 + s
      , toughness = 30 + t
      , initiative = 20 + i
      , agility = 10 + ag
      , dexterity = 30 + dex
      , intelligence = 20 + int'
      , willpower = 40 + wp
      , fellowship = 10 + fel
      , movement  = 3
      , fate = 0
      , resilience = 2
      , race   = "Dwarf"
      , gender = genders gender'
      , age    = 15 + age'
      , place  = worlds r0 r1 r2 places places1 places2
      , eye    = eyes r3
      , hair   = hairs r4
      , height = 51 + r5
      , mark   = pick r6 marks
      , name   = names (genders gender') r7 female male
      , career = careers r8
    }

  -- | Data Tables
  careers :: Int -> String
  careers x = status
    where
      status
        | x <= 9 = "Academic"
        | x <= 34 = "Burgher"
        | x <= 45 = "Courtier"
        | x <= 56 = "Peasant"
        | x <= 67 = "Ranger"
        | x <= 78 = "Riverfolk"
        | x <= 84 = "Rogue"
        | x <= 100 = "Warrior"
        | otherwise = "nil"

  eyes :: Int -> String
  eyes x = color
    where
      color
        | x == 2 = "Coal"
        | x == 3 = "Lead"
        | x == 4 = "Steel"
        | x >= 5 && x <= 7   = "Blue"
        | x >= 8 && x <= 11  = "Earth Brown"
        | x >= 12 && x <= 14 = "Dark Brown"
        | x >= 15 && x <= 17 = "Hazel"
        | x == 18 = "Green"
        | x == 19 = "Coper"
        | x == 20 = "Gold"
        | otherwise = "nil"

  female :: [String]
  female = [ "Anika"
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

  hairs :: Int -> String
  hairs x = color
    where
      color
        | x == 2 = "White"
        | x == 3 = "Grey"
        | x == 4 = "Pale Blond"
        | x >= 5 && x <= 7   = "Golden"
        | x >= 8 && x <= 11  = "Copper"
        | x >= 12 && x <= 14 = "Bronze"
        | x >= 15 && x <= 17 = "Brown"
        | x == 18 = "Dark Brown"
        | x == 19 = "Reddish Brown"
        | x == 20 = "Black"
        | otherwise = "nil"

  male :: [String]
  male = [ "Bardin"
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
