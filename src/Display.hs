module Display where
  import Text.Printf
  import Character
  import Util (wounds, heightF, classes)

  -- | profile
  rpg :: Character -> IO ()
  rpg n = do
    story n
    printf "  | WS | BS | S  | T  | I  | Ag | Dex | Int | Wp | Fel |\n"
    printf "  ------------------------------------------------------\n"
    printf "  | %-2d |" $ weaponSkill n
    printf " %-2d |"    $ ballisticSkill n
    printf " %-2d |"    $ strength n
    printf " %-2d |"    $ toughness n
    printf " %-2d |"    $ initiative n
    printf " %-2d |"    $ agility n
    printf " %-3d |"    $ dexterity n
    printf " %-3d |"    $ intelligence n
    printf " %-2d |"    $ willpower n
    printf " %-3d |\n"  $ fellowship n
    printf "\n"
    skills n
    printf "\n"

  -- | every character has a story...
  story :: Character -> IO ()
  story n = do
    printf "Name: %-16s" (name n)
    printf " | Species: %-8s" (race n)
    printf " | Class: %-8s" (career n)
    printf "\n"
    printf "From: % -16s" (place n)
    printf " | Mark: %-8s" (mark n)
    printf "\n"
    printf "Age:  %-16d" (age n)
    printf " | Height:  %-8s" $ heightF (height n)
    printf " | Hair: %-8s" (hair n)
    printf " | Eyes: %-8s" (eye n)
    printf "\n"
    printf "Fate: %-16d" (fate n)
    printf " | Resilience:  %-4d" (resilience n)
    printf " | Wounds: %-8d" $ wounds (strength n)(toughness n)(willpower n)
    printf "\n"
    printf "\n"

  -- | basic skills
  skills :: Character -> IO ()
  skills n = do
    printf "Basic Skills     | Characteristic |\n"
    printf "-----------------------------------\n"
    printf "Art              | Dex | %d       |\n" (dexterity n)
    printf "Athletics        | Ag  | %d       |\n" (agility n)
    printf "Bribery          | Fel | %d       |\n" (fellowship n)
    printf "Charm            | Fel | %d       |\n" (fellowship n)
    printf "Charm Animal     | WP  | %d       |\n" (willpower n)
    printf "Climb            | S   | %d       |\n" (strength n)
    printf "Cool             | WP  | %d       |\n" (willpower n)
    printf "Consume Alcohol  | T   | %d       |\n" (toughness n)
    printf "Dodge            | Ag  | %d       |\n" (agility n)
    printf "Drive            | Ag  | %d       |\n" (agility n)
    printf "Endurance        | T   | %d       |\n" (toughness n)
    printf "Entertain        | Fel | %d       |\n" (fellowship n)
    printf "Gamble           | Int | %d       |\n" (intelligence n)
    printf "Gossip           | Fel | %d       |\n" (fellowship n)
    printf "Haggle           | Fel | %d       |\n" (fellowship n)
    printf "Intimidate       | S   | %d       |\n" (strength n)
    printf "Intuition        | I   | %d       |\n" (initiative n)
    printf "Leadership       | Fel | %d       |\n" (fellowship n)
    printf "Melee (Basic)    | WS  | %d       |\n" (weaponSkill n)
    printf "Melee            | WS  | %d       |\n" (weaponSkill n)
    printf "Navigation       | I   | %d       |\n" (initiative n)
    printf "Outdoor Survival | Int | %d       |\n" (intelligence n)
    printf "Perception       | I   | %d       |\n" (initiative n)
    printf "Ride             | Ag  | %d       |\n" (agility n)
    printf "Row              | S   | %d       |\n" (strength n)
    printf "Stealth          | Ag  | %d       |\n" (agility n)
