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
  _     -> NullSt

playadv :: HidPlayer -> IO HidPlayer
playadv hpl = do
  newpl <- playturn $ Player_state ["<placeholder>"] [] hpl
  return $ visible newpl

playturn :: Player -> IO Player
playturn pl = do
  print pl
  if not (player_alive pl)
  then
    do
      return pl
  else
    do
      line <- getLine
      let cmd = mparse line
      putStr $ if cmd==Help then help else ""
      if (cmd /= End) 
        then playturn $ make_move pl cmd else return pl
  where
    help = "rcv_dmg <Amount>\ndraw <Amount>\nput <Card>\nmove <Card> <From> <To>\nhelp\nNames: Hand -> Hand, Ing -> In Game, Rfp -> Removed from play, Grv -> Graveyard\n"


credits :: Game -> IO Bool
credits g =
  do
    let ret1 = (player_alive $ player g ) 
    let ret2 = (adversary_alive $ adversary g)
    if ret1 && ret2
    then
      do
        return True
    else
      do
        if ret1
        then 
          do 
            putStr "You Win!\n"
        else
          do
            putStr "You Lose!\n"
        return False

play :: Game -> IO ()
play game = 
  do
    putStr "Adversary turn\n---\n"
    newadv <- playadv $ adversary game
    r1 <- credits (Game_state (player game) newadv)
    if not r1 
    then 
      do
        return ()
    else
      do
        putStr "Your turn\n---\n"
        newpl <- playturn $ player game
        let newg = Game_state newpl newadv
        r2 <- credits newg
        if (not r2)
        then
          do
            return ()
        else
          do    
            play newg
