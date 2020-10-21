module Human(genHuman) where
  import Control.Monad.Random
  import DiceSet
  import Character
  import Util

  -- | human know thy self
  -- | require lots of d10
  genHuman :: IO Character
  genHuman = do
    let die = (1, 10)
        dice = (2, 10)
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
    age' <- evalRandIO (rollDice die)
    r0 <- evalRandIO (rollDice die)
    r1 <- evalRandIO (rollDice die)
    r2 <- evalRandIO (rollDice die)
    r3 <- evalRandIO (rollDice die)
    r4 <- evalRandIO (rollDice die)
    r5 <- evalRandIO (rollDice die)
    r6 <- evalRandIO (rollDice die)
    r7 <- evalRandIO (rollDice die)
    r8 <- evalRandIO (rollDice die)
    r9 <- evalRandIO (rollDice die)
    r10 <- evalRandIO (rollDice die)
    r11 <- evalRandIO (rollDice die)
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
      , race   = "Human"
      , gender = genders age'
      , age    = 15 + age'
      , place  = worlds 1 r0 r1 places places1 places2
      , eye    = pick r2 eyes
      , hair   = pick r3 hairs
      , height = 57 + r4 + r5
      , weight = 100 + (r6 + r7) * 5
      , mark   = pick (r8 + r9) marks
      , name   = names (genders age') (r10 + r11) female male
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
