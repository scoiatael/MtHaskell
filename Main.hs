import Core
import TClient
import Data.Char (toUpper)
main = do
  putStr "Your deck:\n"
  deck <- getLine
  let game = Game_state (Player_state (map (:[]) deck) [] (HidPlayer_state [] [] [] 20)) 
                        (HidPlayer_state [] [] [] 20)
  play game
  putStr "Play again? Y/N\n"
  line <- getLine
  if (toUpper (line !! 0)) == "Y")
  then 
    do
      main
  else
    do
      putStr "Bye then..\n"
