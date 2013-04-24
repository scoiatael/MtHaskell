
module NetworkClient where

import Network
import System.IO
import System.Environment (getProgName)
import Control.Monad.Fix (fix)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import qualified MyIOLib

startGame = startChat 

startChat hostname port (hin, hout) = do
    h <- connectTo hostname port
    hSetBuffering h NoBuffering
    putStr "\n[q]uit to quit.\n"
    sem <- newEmptyMVar
    reader <- forkIO $ mainRWLoop (MyIOLib.handleToInputF h) hout sem
    mainRWLoop hin (MyIOLib.handleToOutputF h) sem
    putStrLn "Bye then."
    killThread reader
    hClose h

mainRWLoop :: MyIOLib.InputF -> MyIOLib.OutputF -> (MVar ()) ->  IO ()
mainRWLoop hin hout sem = fix $ \loop -> do { 
  yield;
  mstring <- hin; 
  process mstring;
  cont <- isEmptyMVar sem;
  when cont loop }
  where
    process Nothing = putStr "" 
    process (Just cont) = do { 
      when (cont == "quit") $ do { void $ tryTakeMVar sem; putMVar sem (); };
      hout cont; }; 

printUsage name = putStr (name ++ " { chat | game } <hostname> <port>\n")

