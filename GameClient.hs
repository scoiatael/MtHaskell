module GameClient where

import GameCore
import MtGClient (help_desc, mparse, player_alive, doClientC, ClientC, newPlayer, newAdversary, validateMove)
import qualified Data.Map as DMap
import Data.List (unfoldr)

-- placeholders
playadv :: HidPlayer -> IO HidPlayer
playadv hpl = do
  newpl <- playturn $ Player_state [["<placeholder>"]] [] hpl
  return $ visible newpl

adversary_alive :: HidPlayer -> Bool
adversary_alive hpl = (hp hpl) > 0
--

playturn :: Player -> IO Player
playturn pl = do
  print pl
  if not (player_alive pl)
  then
    do { return pl; }
  else
    do {
      cmd <- get_valid_command pl;
      (npl, cont) <- (doCommand doClientC) cmd pl
      ; if cont 
          then
            do { playturn npl; }
          else
            do { return npl; }
    }

get_valid_command :: Player -> IO ClientC
get_valid_command pl = do {
  line <- getLine
  ; let cmd = mparse line
  ; print cmd
  ; case (doCommand validateMove) cmd pl of
          Nothing -> do { return cmd; }
          Just str -> do { putStr str; get_valid_command pl; }
  }

credits :: Game -> IO Bool
credits g =
  do
    let ret1 = (player_alive $ player g ) 
    let ret2 = (adversary_alive $ adversary g)
    if ret1 && ret2
    then
      do { return True; }
    else
      do
        if ret1
        then 
          do { putStr "You Win!\n"; }
        else
          do { putStr "You Lose!\n";}
        return False

play :: Game -> IO ()
play game = 
  do
    putStr "Adversary turn\n---\n"
    let newadv = adversary game
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

