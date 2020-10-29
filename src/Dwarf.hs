module Dwarf (genDwarf) where
  import DiceSet (d10, twoD10, d100)
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
    r0 <- d10
    r1 <- d10
    r2 <- d10
    r3 <- d10
    r4 <- d10
    r5 <- d10
    r6 <- d10
    r7 <- d10
    r8 <- d10
    r9 <- d10
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
      , gender = genders age'
      , age    = 15 + age'
      , place  = worlds r0 r1 r2 places places1 places2
      , eye    = pick r3 eyes
      , hair   = pick r4 hairs
      , height = 51 + r5
      , mark   = pick (r6 + r7) marks
      , name   = names (genders age') (r8 + r9) female male
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
