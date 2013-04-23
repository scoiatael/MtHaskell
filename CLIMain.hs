module CLIMain where

import GameClient
import Data.Char (toUpper)
import NetworkClient
import qualified Server
import qualified Client
import System.Environment
import System.IO (stdin, stdout)
import MyIOLib
import Network
import Control.Monad (when)

main :: String -> [String] -> IO ()
main progName args = do
  if (length args) < 3 then printUsage progName else do 
    let ctype = args !! 1
    let stype = args !! 0
    let hout = stdinToClientConnection
    when ( stype == "client") $ do {
      let hostname = (args !! 2)
      if (length args) < 4 then printUsage progName else do {
        let port = (PortNumber $ toEnum (read (args !! 3) :: Int));
        if ( ctype == "chat") then Client.mainChat hout hostname port;
          else Client.mainGame hout hostname port; }; }
    when ( stype == "server") $ do {
      let port = (PortNumber $ toEnum (read (args !! 2) :: Int))
      if ( ctype == "chat") then Server.mainChat hout port;
        else Server.mainGame hout port; }

{--
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
--}
