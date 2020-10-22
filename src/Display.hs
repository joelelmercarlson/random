module Display where
  import Text.Printf
  import Character
  import Util (wounds, heightF, classes)

  -- | profile
  rpg :: Character -> IO ()
  rpg n = do
    story n
    printf "Profile\n"
    printf "| M  | WS | BS | S  | T  | I  | Ag | Dex | Int | Wp | Fel | W    |\n"
    printf "------------------------------------------------------------------\n"
    printf "| %-2d |"  $ movement n
    printf " %-2d |"   $ weaponSkill n
    printf " %-2d |"   $ ballisticSkill n
    printf " %-2d |"   $ strength n
    printf " %-2d |"   $ toughness n
    printf " %-2d |"   $ initiative n
    printf " %-2d |"   $ agility n
    printf " %-2d  |"  $ dexterity n
    printf " %-2d  |"  $ intelligence n
    printf " %-2d |"   $ willpower n
    printf " %-2d  |"  $ fellowship n
    printf " %-4d |\n" $ wounds (race n)(strength n)(toughness n)(willpower n)
    printf "\n"

  -- | every character has a story...
  story :: Character -> IO ()
  story n = do
    let ht = heightF (height n)
        career = classes (weaponSkill n)(ballisticSkill n)(agility n)(dexterity n)(intelligence n)(willpower n)
    printf "%s, the %s from %s\n" (name n)(race n)(place n)
    printf "  | Age:    %-8d | Height: %-12s\n" (age n) ht
    printf "  | Gender: %-8s | Hair:   %-12s | Eyes: %s\n" (gender n)(hair n)(eye n)
    printf "  | Fate:   %-8d | Mark:   %s\n" (fate n)(mark n)
    printf "  | Class:  %s\n" career
