module Display ( rpg ) where
  import Text.Printf

  import Character
  import Util (clamp, heightF)

  -- | profile
  rpg :: Character -> IO ()
  rpg n = do
    let ld = clamp $ (wp n + fel n) `div` 10
        i  = clamp $ ag n `div` 10
    story n
    printf "Profile\n"
    printf "  | M  | WS | BS | S  | T  | W  | I  | A  | Ag | Int | Wp | Fel | Ld  |\n"
    printf "  | %-2d | %-2d | %-2d | %-2d | %-2d | %-2d | %-2d | %-2d | %-2d | %-3d | %-2d | %-3d | %-3d |\n" (m n) (ws n) (bs n) (s n) (t n) (w n) i (a n) (ag n) (int n) (wp n) (fel n) ld
    printf "\n"

  -- | lots of `div`
  battle :: Character -> IO ()
  battle n = do
    let ws_b = clamp $ ws n `div` 10
        bs_b = clamp $ bs n `div` 10
        s_b  = clamp $ s n  `div` 10
        t_b  = clamp $ t n  `div` 10
        w_b  = clamp $ w n  `div` 5
        i_b  = clamp $ ag n `div` 10
        ld_b = clamp $ (wp n + fel n) `div` 10
    printf "Battle Profile\n"
    printf "  | M  | WS | BS | S  | T  | W  | I  | A  | Ld |\n"
    printf "  | %-2d | %-2d | %-2d | %-2d | %-2d | %-2d | %-2d | %-2d | %-2d |\n" (m n) ws_b bs_b s_b t_b w_b i_b (a n) ld_b
    printf "\n"

  -- | every character has a story...
  story :: Character -> IO ()
  story n = do
    let ht = heightF (height n)
    printf "%s, the %s from %s\n"                            (name n)(race n)(place n)
    printf "  | Age:    %-8d | Height: %-12s | Weight: %d\n" (age n) ht (weight n)
    printf "  | Gender: %-8s | Hair:   %-12s | Eyes: %s\n"   (gender n)(hair n)(eye n)
    printf "  | Fate:   %-8d | Mark:   %s\n"                 (fp n)(mark n)
    printf "  | Rank:   %s\n"                                (career n)
