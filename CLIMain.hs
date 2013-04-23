module CLIMain where

import GameClient
import Data.Char (toUpper)
import NetworkClient
import System.Environment
import MyIOLib
import Network
import Control.Monad (when)

main :: [String] -> MyIOLib.InputF -> MyIOLib.OutputF -> IO ()
main args hin hout = do
  if (length args) < 3 then do { name <- getProgName; printUsage name;} else do 
    let hostname = (args !! 1)
    let port = (PortNumber $ toEnum (read (args !! 2) :: Int))
    let ctype = args !! 0
    when ( ctype == "chat") $ startChat hostname port (hin, hout)
    when ( ctype == "game") $ startGame hostname port (hin, hout)

  newgame <- startNewGame
  play newgame
  putStr "Play again? Y/N\n"
  line <- getLine
  if (toUpper (line !! 0)) == 'Y'
    then 
      do
        main args hin hout
    else
      do
        putStr "Bye then..\n"
