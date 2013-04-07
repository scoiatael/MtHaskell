module TClient where
import Core
import qualified Data.Map as DMap
import Data.List (unfoldr)

-- MtG specific - whole module
empty_played::[DMap.Map Int Played]
empty_played = [DMap.empty, DMap.empty]

newAdversary:: HidPlayer
newAdversary = HidPlayer_state empty_played [[]] 20

newPlayer :: String -> IO Player
newPlayer str = 
  do
    return $ Player_state [map (:[]) str] [[]] newAdversary


adversary_alive :: HidPlayer -> Bool
adversary_alive hpl = (hp hpl) > 0

player_alive :: Player -> Bool
player_alive pl = (hp $ visible pl) > 0
--player_alive pl = (adversary_alive $ visible pl) && (length $ (lib pl)!!0) > 0
     
 {- -rework to communicate with server later
type HidPlayer_move = HidPlayer -> CoreCommand -> HidPlayer
make_shadow_move :: HidPlayer_move
make_shadow_move arg cmd = visible $ doCoreCommand (Player_state [] [] arg) cmd
 -}

data ClientC = Core CoreC | End | Help | Shuffle Int deriving (Show, Eq)

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


-- placeholder
playadv :: HidPlayer -> IO HidPlayer
playadv hpl = do
  newpl <- playturn $ Player_state [["<placeholder>"]] [] hpl
  return $ visible newpl

doClientC :: Command ClientC (IO (Player, Bool))
doClientC = Cmd (\cmd -> \pl ->
  case cmd of 
    End       -> do { return (pl, False); }
    Help      -> do { putStr help_desc; return (pl, True); }
    Shuffle i -> do { let npl = shuffle pl i in return (npl, True); }
    Core ccmd -> do { return ((doCommand doCoreC) ccmd pl, True); }
  )

playturn :: Player -> IO Player
playturn pl = do
  print pl
  if not (player_alive pl)
  then
    do { return pl; }
  else
    do
      line <- getLine
      let cmd = mparse line
      print cmd
      (npl, cont) <- (doCommand doClientC) cmd pl
      if cont 
      then
        do { playturn npl; }
      else
        do { return npl; }

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

startNewGame :: IO Game
startNewGame = do
  putStr "Your deck:\n"
  deckname <- getLine
  player <- newPlayer deckname
  let game = Game_state (player) 
                        (newAdversary)
  putStr help_desc
  return game

shuffle :: Player -> Int -> Player
shuffle pl hash = pl { lib = map (randomShuffleList hash) (lib pl) }

randomShuffleList :: Int -> [a] -> [a]
randomShuffleList h ls = let lls = length ls in foldl (flip auxswap) ls (genRandomNumberList h lls)

genRandomNumberList :: Int -> Int -> [Int]
genRandomNumberList h lls =  map (`mod` (lls-1)) $ take lls $ unfoldr (\x -> Just (x, (1664525*x + 1013904223) `mod `(2^32))) (h`mod`(2^32))

auxswap :: Int -> [a] -> [a]
auxswap n [] = []
auxswap n (x:xs) = let (l,(r:rs)) = splitAt n xs in r:(l ++ (x:rs))
