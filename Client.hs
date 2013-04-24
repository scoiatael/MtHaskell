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

startGame :: ServerConnection -> ClientConnection -> IO ClientReaction
startGame sconn cconn = do {
  gsptr <- newEmptyMVar;
--  playerDeck <- getPlayerDeck cconn;
--  advDeck <- getAdvDeck sconn;
--  putMVar gsptr $ gameState (playerFromDeck playerDeck, advFromDeck advDeck);
  return $ CR $ onClientInput sconn cconn gsptr }

mainChat :: HostName -> PortID -> ClientConnection -> IO ClientReaction
mainChat host port hout = do
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
    process Nothing = putStr ""
    process (Just cont) = do { 
      when (cont == "quit") $ do { void $ tryTakeMVar sem; putMVar sem (); };
      hout cont; }; 

printUsage name = putStr (name ++ " { chat | game } <hostname> <port>\n")

--Reaction to input client side
onClientInput :: ServerConnection -> ClientConnection -> MVar Core.GameState -> Input -> IO ()
onClientInput servhandle clienthandle gsptr inpt = do {
  putStr "";
{--  gs <- readMVar gsptr;
  let cmd = parseClientInput input;
  scmd <- executeClientCmd serverhandle clienthandle cmd;
  case checkIfValidMove scmd of 
    Right gdcmd -> do { send servhandle $ gdcmd; }
    Left errorstr -> do { send clienthandle $ errorstr; };--} } 
{--
--Reaction to input server side
onServerInput :: ServerConnection -> ClientConnection -> MVar GameState -> Input -> IO ()
onServerInput servhandle clienthandle gsptr inpt = do {
  gs <- takeMVar gsptr;
  let cmd = parseServerInput input;
  gccmd <- executeServerCmd serverhandle clienthandle cmd;
  let newgs = executeGameMove gs cmd;
  putMVar gsptr newgs; }
--}
