module Display ( rpg
               , rpg'
               ) where
  import Text.Printf

  import Character
  import Util (height_f)

  rpg :: Character -> IO ()
  rpg n = do
    story n
    printf "Main Profile\n"
    printf "   | WS | BS | S  | T  | W  | Ag | Int | Wp | Fel |\n"
    printf "   | %-2d | %-2d | %-2d | %-2d | %-2d | %-2d | %-3d | %-2d | %-3d |\n" (ws n)(bs n)(s n)(t n)(w n)(ag n)(int n)(wp n)(fel n)
    war n

  rpg' :: Character -> IO ()
  rpg' n = do
    let s_b = (s n) `div` 10
        t_b = (t n) `div` 10
    story n
    printf "Main Profile\n"
    printf "   | WS | BS | S  | T  | Ag | Int | Wp | Fel |\n"
    printf "   | %-2d | %-2d | %-2d | %-2d | %-2d | %-3d | %-2d | %-3d |\n" (ws n)(bs n)(s n)(t n)(ag n)(int n)(wp n)(fel n)
    printf "Secondary Profile\n"
    printf "   | A  | W  | SB | TB | M  | Mag | IP | FP  |\n"
    printf "   | %-2d | %-2d | %-2d | %-2d | %-2d | %-3d | %-2d | %-3d |\n" (a n)(w n) s_b t_b (m n)(mag n)(ip n)(fp n)

  war :: Character -> IO ()
  war n = do
    let ws_b = (ws n) `div` 10
        bs_b = (bs n) `div` 10
        s_b  = (s n)  `div` 10
        t_b  = (t n)  `div` 10
        i_b  = (ag n) `div` 10
        w_b  = (w n)  `div` 5
        ld_b = (ld n) `div` 10
    printf "Battle\n"
    printf "   | M  | WS | BS | S  | T  | W  | I  | A  | Ld |\n"
    printf "   | %-2d | %-2d | %-2d | %-2d | %-2d | %-2d | %-2d | %-2d | %-2d |\n" (m n) ws_b bs_b s_b t_b w_b i_b (a n) ld_b

  story :: Character -> IO ()
  story n = do
    let ht = (height_f (height n))
    printf "%s, the %s from %s\n\n" (name n)(race n)(place n)
    printf " Age:    %-16d | Height: %-16s | Weight: %-12d\n" (age n) ht (weight n)
    printf " Gender: %-16s | Hair:   %-16s | Eyes: %-12s\n" (gender n)(hair n)(eye n)
    printf " Mark:   %-16s | Fate: %-8d\n\n" (mark n)(fp n)
