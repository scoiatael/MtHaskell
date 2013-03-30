import Core
import TClient
import Data.Char (toUpper)
main = do
  putStr "Your deck:\n"
  deck <- getLine
  player <- newPlayer deck
  let game = Game_state (player) 
                        (newAdversary)
  putStr help_desc
  play game
  putStr "Play again? Y/N\n"
  line <- getLine
  if (toUpper (line !! 0)) == 'Y'
  then 
    do
      main
  else
    do
      putStr "Bye then..\n"
