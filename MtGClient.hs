module MtGClient where
import Core

-- MtG specific - whole module
empty_played = [empty, empty]

newAdversary:: HidPlayer
newAdversary = HidPlayer_state empty_played [[]] 20

newPlayer :: String -> IO Player
newPlayer str = 
  do
    return $ Player_state [map (:[]) str] [[]] newAdversary

player_alive :: Player -> Bool
player_alive pl = (hp $ visible pl) > 0


data ClientC = Core CoreC | End | Help | Shuffle Int deriving (Show, Eq)

doClientC :: Command ClientC (IO (Player, Bool))
doClientC = Cmd (\cmd -> \pl ->
  case cmd of 
    End       -> do { return (pl, False); }
    Help      -> do { putStr help_desc; return (pl, True); }
    Shuffle i -> do { let npl = shuffle pl i in return (npl, True); }
    Core ccmd -> do { return ((doCommand doCoreC) ccmd pl, True); }
  )

mparse :: String -> ClientC
mparse str = let wrds = words str in case wrds !! 0 of
  "suffer" -> Core $ Receive_dmg (read (wrds !! 1) :: Int)
  "draw" -> Core $ From_lib 0 (read (wrds !! 1) :: Int) (Hand, 0)
  "move" -> Core $ Move_card (cparse $ wrds !! 1) (sparse $ wrds !! 2) (sparse $ wrds !! 3)
  "help" -> Help
  "end" -> End
  "shuffle" -> Shuffle (read (wrds !! 1) :: Int)
  otherwise -> Core $ NullC

cparse :: String -> CardId
cparse str = case str !! 0 of
  'i' -> Id (read (tail str) :: Int)
  otherwise -> Card str

sparse :: String -> Stack
sparse str = case str of
  "Lib" -> (Lib,0)
  "Hand"-> (Hand,0)
  "Ing" -> (VPlayed,0)
  "Rfp" -> (VPlayed, 1)
  "Grv" -> (VCards,0)
  _     -> (NullS,0)

help_desc = "suffer <Amount>\ndraw <Amount>\nmove <Card> <From> <To>\nhelp\nend - end turn\nshuffle <Int> - shuffle library with hash\nNames: Hand -> Hand, Ing -> In Game, Rfp -> Removed from play, Grv -> Graveyard\n"
