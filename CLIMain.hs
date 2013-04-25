module CLIMain where

import qualified Server
import qualified Client
import MyIOLib

import Data.Char (toUpper)
import System.Environment
import System.IO (stdin, stdout)
import Network
import Control.Monad (when)
import Control.Monad.Fix (fix)

main :: String -> [String] -> IO ()
main progName args = do
  if (length args) < 3 then printUsage progName else do 
    let stype = args !! 0
    when ( stype == "client") $ if (length args) < 4 then printUsage progName else clientPart
    when ( stype == "server") serverPart
  where
    clientPart = do
      putStrLn "CLI client starting..."
      let hout = stdoutToClientConnection
      let ctype = args !! 1
      let hostname = args !! 2
      let port = (PortNumber $ toEnum (read (args !! 3) :: Int))
      if ( ctype == "chat") 
        then mainCChat hout hostname port
        else mainCGame hout hostname port
    serverPart = do
      putStrLn "CLI server starting..."
      let hout = stdoutToClientConnection
      let ctype = args !! 1
      let stype = args !! 0
      let port = (PortNumber $ toEnum (read (args !! 2) :: Int))
      if ( ctype == "chat") 
        then Server.mainChat hout port
        else Server.mainGame hout port

mainCChat hout hostname port = do
  putStrLn "So far here..1"
  creact <- Client.mainChat hout hostname port
  fix $ \loop -> do { line <- getLine; cdoReact creact $ line; loop }

mainCGame _ _ _ = putStrLn "So far here..2"

printUsage name = putStr (name ++ "{ server | client } { chat | game } [client -> <hostname>] <port>\n")

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
