module Client where 

import Network
import System.IO
import System.Environment (getProgName)
import Control.Monad.Fix (fix)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import MyIOLib
import Core
import MtGClient

mainGame :: ClientConnection -> HostName -> PortID -> IO ClientReaction
mainGame cconn host port = do 
  h <- connectTo host port
  hSetBuffering h NoBuffering
  startChat (handleToServerConnection h) cconn 

startGame :: ServerConnection -> ClientConnection -> IO ClientReaction
startGame sconn cconn = do {
  gsptr <- newEmptyMVar;
  playerDeck <- getPlayerDeck cconn;
  advDeck <- getAdvDeck sconn;
  putMVar gsptr (newPlayer playerDeck, newAdversary advDeck);
  return $ CR $ onClientInput sconn cconn gsptr }

getPlayerDeck cconn = do
  (csend cconn) "Your deck:\n"
  cget cconn

getAdvDeck sconn = do
  (ssend sconn) "Deck?"
  sget sconn

mainChat :: ClientConnection -> HostName -> PortID -> IO ClientReaction
mainChat hout host port = do
  h <- connectTo host port
  hSetBuffering h NoBuffering
  startChat (handleToServerConnection h) hout 
  
startChat :: ServerConnection -> ClientConnection -> IO ClientReaction
startChat sconn cconn = do
  csend cconn $ "\n[q]uit to quit.\n"
  sem <- newEmptyMVar
  reader <- forkIO $ mainRWLoop (sget sconn) (csend cconn) sem
  return $ CR $ ssend sconn

mainRWLoop :: MyIOLib.InputF -> MyIOLib.OutputF -> (MVar ()) ->  IO ()
mainRWLoop hin hout sem = fix $ \loop -> do { 
  yield;
  mstring <- hin; 
  process mstring;
  cont <- isEmptyMVar sem;
  when cont loop }
  where
    process cont = do { 
      when (cont == "quit") $ do { void $ tryTakeMVar sem; putMVar sem (); };
      hout cont; }; 


--Reaction to input client side
onClientInput :: ServerConnection -> ClientConnection -> MVar Core.GameState -> Input -> IO ()
onClientInput servhandle clienthandle gsptr inpt = do {
  putStr ""; }

get_valid_command :: Player -> IO ClientC
get_valid_command pl = do {
  line <- getLine
  ; let cmd = mparse line
  ; print cmd
  ; case (doCommand validateMove) cmd pl of
          Nothing -> do { return cmd; }
          Just str -> do { putStr str; get_valid_command pl; }
  }
