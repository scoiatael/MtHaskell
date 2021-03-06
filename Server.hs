 
module Server where
import Network
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Fix (fix)
import System.Environment (getArgs, getProgName)

import MyIOLib
 
type Msg = (Int, String)
{-- 
main :: IO ()
main = do
    args <- getArgs
    print args
    when ((length args) < 2) $ do { printUsage; return (); }
    let stype = args!!0
    let portnr = (PortNumber (toEnum (read (args !! 1) :: Int)))
    when (stype=="chat") $
        mainChat portnr
    when (stype=="game") $
        mainGame portnr
--}
{--
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
    listen sock 2
--}

mainGame cconn port = do
  (csend cconn) "Starting game server..."
  sock <- listenOn port
  (hdl1, _, _) <- accept sock
  (csend cconn) "Got first player"
  hSetBuffering hdl1 NoBuffering
  player1 <- loadPlayer hdl1
  (hdl2, _, _) <- accept sock
  (csend cconn) "Got second player"
  hSetBuffering hdl2 NoBuffering
  player2 <- loadPlayer hdl2
  startGame cconn (player1, hdl1) (player2, hdl2)

loadPlayer hdl = return ""

startGame cconn (pl1, hdl1) (pl2, hdl2) = do
  sem <- newEmptyMVar
  thr1 <- simplicityWrap (pl1, hdl1) hdl2 sem "Player1"
  (csend cconn) ("Player1: " ++ (show thr1))
  thr2 <- simplicityWrap (pl2, hdl2) hdl1 sem "Player2"
  (csend cconn) ("Player2: " ++ (show thr2))
  void $ takeMVar sem
  (csend cconn) "Ending game.."
  safeKillThread thr1
  safeKillThread thr2
  endConnection hdl1;
  endConnection hdl2;
  where
    simplicityWrap phdl hdl sem pl = 
      forkIO $ do {forwardInfo phdl hdl sem; (csend cconn) (pl ++ " left"); putMVar sem();} `catch` exceptionHandler
    safeKillThread tid = do { (csend cconn) ("Killing " ++ (show tid)); killThread tid;}

forwardInfo (fpl, fhdl) thdl sem = do
  hPutStrLn fhdl "ping"
  move <- hGetLine fhdl
  putStrLn move 
  let (fpl', okmove) = validMove fpl move
  when okmove $ hPutStrLn thdl move
  when (not okmove) $ hPutStrLn fhdl "bad move"
  when (move /= "quit") $ forwardInfo (fpl', fhdl) thdl sem

validMove _ _ = ("", True)
  
mainChat cconn port = do
    (csend cconn) "Starting chat server.."
    chan <- newChan
    sock <- listenOn port
    forkIO $ fix $ \loop -> do
        (_, msg) <- readChan chan
        (csend cconn) msg
        loop
    mainLoop sock chan 0
 
printUsage = do { prog <- getProgName; putStr ("Usage: " ++ prog ++ "{ chat | game } <port>\n"); }

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan nr = do
    conn <- accept sock
    forkIO (runConn conn chan nr)
    mainLoop sock chan $! nr+1
 
runConn :: (Handle, HostName, PortNumber) -> Chan Msg -> Int -> IO ()
runConn (hdl, _, _) chan nr = do
    let broadcast msg = do { writeChan chan (nr, msg) }
    broadcast ("New connection. " ++ show nr)
    hSetBuffering hdl NoBuffering
    hPutStr hdl "Hi, what's your name?\n"
    name <- hGetLine hdl
    broadcast ("--> " ++ name ++ " entered.")
    hPutStr hdl ("Welcome, " ++ name ++ "!\n")
    hFlush hdl;
    chan' <- dupChan chan
    reader <- forkIO $ fix $ \loop -> do
        (nr', line) <- readChan chan'
        when (nr /= nr') $ do { hPutStr hdl (line++['\n']); hFlush hdl; print nr; }
        loop
    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- hGetLine hdl
        case line of
         "quit" -> do { hPutStrLn hdl "Bye!\n"; hFlush hdl; }
         _      -> do
            broadcast (name ++ ": " ++ line)
            loop
    killThread reader
    broadcast ("<-- " ++ name ++ " left.")
    hClose hdl
