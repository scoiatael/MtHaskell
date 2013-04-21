module CLIMain where

import GameClient
import Data.Char (toUpper)

main = do
  newgame <- startNewGame
  play newgame
  putStr "Play again? Y/N\n"
  line <- getLine
  if (toUpper (line !! 0)) == 'Y'
  then 
    do
      main
  else
    do
      putStr "Bye then..\n"
