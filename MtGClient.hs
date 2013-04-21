module MtGClient where
import GameCore

-- MtG specific - whole module
empty_played = [empty, empty]

instance Show Player where
  show pl = "==>\n" ++ (show $ length ((lib pl) !! 0)) ++ " cards in Library\nHand: " ++ (show $ (hand pl) !! 0) ++ "\n--\nVisible: " ++ (show $ visible pl) ++ "<==\n"

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
  "suffer" -> Core $ Receive_dmg (read $ wrds !! 1 :: Int)
  "draw" -> Core $ From_lib 0 (read $ wrds !! 1 :: Int) (Hand, 0)
  "move" -> Core $ Move_card (cparse $ wrds !! 1) (sparse $ wrds !! 2) (sparse $ wrds !! 3)
  "help" -> Help
  "end" -> End
  "shuffle" -> Shuffle (read $ wrds !! 1 :: Int)
  "atoken" -> Core $ Add_token (read $ wrds !! 1 :: Int) (wrds !! 2)
  "tap" -> Core $ Tap (read $ wrds !! 1 :: Int) (cparse $ wrds !! 2) 0
  "untap" -> Core $ Untap (read $ wrds !! 1 :: Int) (cparse $ wrds !! 2) 0
  "acounter" -> Core $ Add_counter (read $ wrds !! 1 :: Int) (cparse $ wrds !! 2) (wrds !! 3)
  "dcounter" -> Core $ Del_counter (read $ wrds !! 1 :: Int) (cparse $ wrds !! 2) (wrds !! 3)
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

help_desc = "suffer <Amount>\ndraw <Amount>\nmove <Card> <From> <To>\nhelp\nend\nshuffle <Int>\natoken <To> <Name>\nmstate <Int> <Card> <NewState>\nNames: Hand -> Hand, Ing -> In Game, Rfp -> Removed from play, Grv -> Graveyard\n"

validateMove :: Command ClientC (Maybe String)
validateMove = Cmd (\cmd -> \pl -> 
  case cmd of 
    Core ccmd -> case ccmd of
      Tap ws wc _ -> target_exists wc (VPlayed, ws) pl
      Untap ws wc  _-> target_exists wc (VPlayed, ws) pl
      Add_counter ws wc _ -> target_exists wc (VPlayed, ws) pl
      Del_counter ws wc _ -> target_exists wc (VPlayed, ws) pl
      Add_token ws _ -> if ws > 1 then Just "No such stack.\n" else Nothing
      From_lib wl wm _ -> if (length ((lib pl) !! wl)) < wm then Just "Not enough cards in library.\n" else Nothing
      Move_card wc f _ -> target_exists wc f pl
      otherwise -> Nothing
    otherwise -> Nothing
  )

target_exists :: CardId -> Stack -> Player -> Maybe String
target_exists cid st pl = 
  case st of 
    (VPlayed, i) 
      -> if i > 1 then Just "No such stack.\n" else if (getplayedid cid) `member` ((vplayed $ visible pl)!!i) then Nothing else Just "No such target.\n"
    (VCards, i) 
      -> if i > 0 then Just "No such stack.\n" else if (getcardname cid) `elem` ((vcards $ visible pl)!!i) then Nothing else Just "No such target.\n"
    (Hand, i) 
      -> if i > 0 then Just "No such stack.\n" else if (getcardname cid) `elem` ((hand pl)!!i) then Nothing else Just "No such target.\n"
    otherwise 
      -> Just "Bad stack.\n"
