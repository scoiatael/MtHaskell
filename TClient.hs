module TClient where
import Core

mparse :: String -> Command
mparse str = let wrds = words str in case wrds !! 0 of
  "rcv_dmg" -> Receive_dmg (read (wrds !! 1) :: Int)
  "draw" -> From_lib (read (wrds !! 1) :: Int)
  "put" -> To_lib (wrds !! 1)
  "move" -> Move_card (wrds !! 1) (sparse $ wrds !! 2) (sparse $ wrds !! 3)
  "help" -> Help
  "end" -> End
  otherwise -> Null

sparse :: String -> Stack
sparse str = case str of
  "Lib" -> Lib
  "Hand"-> Hand
  "Ing" -> Ing
  "Rfp" -> Rfp
  "Grv" -> Grv

playadv :: HidPlayer -> IO HidPlayer
playadv hpl = do
  putStr "Adversary turn\n"
  newpl <- playturn $ Player_state [] [] hpl
  return $ visible newpl

playturn :: Player -> IO Player
playturn pl = do
  putStr "Your turn\n"
  print pl
  line <- getLine
  let cmd = mparse line
  putStr $ if cmd==Help then help else ""
  if (cmd /= End) && (player_alive pl)
    then playturn $ make_move pl cmd else return pl
  where
    help = "rcv_dmg <Amount>\ndraw <Amount>\nput <Card>\nmove <Card> <From> <To>\nhelp\nNames: Hand -> Hand, Ing -> In Game, Rfp -> Removed from play, Grv -> Graveyard\n"


play :: Game -> IO ()
play game = 
  do
    if (player_alive $ player game ) && (adversary_alive $ adversary game)
    then 
      do
        newadv <- playadv $ adversary game
        newpl <- playturn $ player game
        let newg = Game_state newpl newadv
        play newg
    else
      do
        if not $ player_alive $ player game
        then
          do
            putStr "You Lose!\n"
        else
          do
            putStr "You Win!\n"
