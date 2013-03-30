module TClient where
import Core

newAdversary:: HidPlayer
newAdversary = HidPlayer_state [empty_played] [[]] 20

newPlayer :: String -> IO Player
newPlayer str = 
  do
    return $ Player_state [map (:[]) str] [[]] newAdversary

adversary_alive :: HidPlayer -> Bool
adversary_alive hpl = (hp hpl) > 0

player_alive :: Player -> Bool
player_alive _ = True
--player_alive pl = (adversary_alive $ visible pl) && (length $ (lib pl)!!0) > 0
     
shuffle :: [Card] -> [Card]
shuffle r = r


type HidPlayer_move = HidPlayer -> CoreCommand -> HidPlayer
make_shadow_move :: HidPlayer_move
make_shadow_move arg cmd = visible $ make_move (Player_state [] [] arg) cmd

data ClientCommand = Core CoreCommand | End | Help deriving (Eq)
mparse :: String -> ClientCommand
mparse str = let wrds = words str in case wrds !! 0 of
  "suffer" -> Core $ Receive_dmg (read (wrds !! 1) :: Int)
  "draw" -> Core $ From_lib (read (wrds !! 1) :: Int) 0 (Hand, 0)
  "move" -> Core $ Move_card (cparse $ wrds !! 1) (sparse $ wrds !! 2) (sparse $ wrds !! 3)
  "help" -> Help
  "end" -> End
  otherwise -> Core $ NullC

cparse :: String -> CardId
cparse str = case str!!0 of
  i -> Id (read (tail str) :: Int)
  _ -> Card str

sparse :: String -> Stack
sparse str = case str of
  "Lib" -> (Lib,0)
  "Hand"-> (Hand,0)
  "Play" -> (VPlayed,0)
  "Rfp" -> (VPlayed, 1)
  "Grave" -> (VCards,0)
  _     -> (NullS,0)

playadv :: HidPlayer -> IO HidPlayer
playadv hpl = do
  newpl <- playturn $ Player_state [["<placeholder>"]] [] hpl
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
      putStr $ if cmd==Help then help_desc else ""
      if cmd == End then return pl
                    else let Core ccmd = cmd in playturn $ make_move pl ccmd

help_desc = "suffer <Amount>\ndraw <Amount>\nmove <Card> <From> <To>\nhelp\nend - end turn\nNames: Hand -> Hand, Ing -> In Game, Rfp -> Removed from play, Grv -> Graveyard\n"


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
