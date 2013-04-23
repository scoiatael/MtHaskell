module Client where 

import Network
import System.IO
import System.Environment (getProgName)
import Control.Monad.Fix (fix)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import qualified MyIOLib

type Input = String
data Connection = Conn { send :: Input -> IO (), quit :: IO () }
type ServerConnection = Connection
type ClientConnection = Connection
type GameState = (PlayerState, AdvState)

type Reaction = Input -> IO ()
newtype ClientReaction = CR Reaction
newtype ServerReaction = SR Reaction 

startGame :: ServerConnection -> ClientConnection -> IO ClientReaction
startGame sconn cconn = do {
  gsptr <- newEmptyMVar GameState;
  playerDeck <- getPlayerDeck cconn;
  advDeck <- getAdvDeck sconn;
  putMVar gsptr $ gameState (playerFromDeck playerDeck, advFromDeck advDeck);
  return $ CR $ onClientInput sconn cconn gsptr }

startChat :: ServerConnection -> ClientConnection -> IO ClientReaction
startChat sconn cconn = do
    send cconn $ "\n[q]uit to quit.\n"
    sem <- newEmptyMVar
    reader <- forkIO $ mainRWLoop (MyIOLib.handleToInputF h) hout sem
    mainRWLoop hin (MyIOLib.handleToOutputF h) sem
    putStrLn "Bye then."
    killThread reader
    hClose h

mainRWLoop :: MyIOLib.InputF -> MyIOLib.OutputF -> (MVar ()) ->  IO ()
mainRWLoop hin hout sem = fix $ \loop -> do { 
  yield;
  mstring <- hin 100; 
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
onClientInput :: ServerConnection -> ClientConnection -> MVar GameState -> Input -> IO ()
onClientInput servhandle clienthandle gsptr inpt = do {
  gs <- readMVar gsptr;
  let cmd = parseClientInput input;
  scmd <- executeClientCmd serverhandle clienthandle cmd;
  case checkIfValidMove scmd of 
    Right gdcmd -> do { send servhandle $ gdcmd; }
    Left errorstr -> do { send clienthandle $ errorstr; }; }
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
