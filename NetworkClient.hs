import Network
import System.IO
import System.Environment (getArgs, getProgName)
import Control.Monad.Fix (fix)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar

module NetworkClient where

main = withSocketsDo $ do
  args <- getArgs
  if (length args) < 3 then do { name <- getProgName; printUsage name;} else do 
    let hostname = (args !! 1)
    let port = (PortNumber $ toEnum (read (args !! 2) :: Int))
    let ctype = args !! 0
    when ( ctype == "chat") $ startChat hostname port
    when ( ctype == "game") $ startGame hostname port

startGame = startChat 

startChat hostname port = do
    h <- connectTo hostname port
    hSetBuffering h NoBuffering
    putStr "\n[q]uit to quit.\n"
    sem <- newEmptyMVar
    reader <- forkIO $ mainReaderLoop h sem
    mainWriterLoop h sem
    putStrLn "Bye then."
    killThread reader
    hClose h

mainRWLoop h1 h2 sem = fix $ \loop -> do { 
  yield;
  ready <- hWaitForInput h1 100; 
  when ready $
   do { 
      cont <- hGetLine h1; 
      when (cont == "quit") $ do { void $ tryTakeMVar sem; putMVar sem (); };
      hPutStrLn h2 cont; 
      hFlush h2; }; 
  cont <- isEmptyMVar sem;
  when cont loop }

mainReaderLoop h sem = mainRWLoop h stdout sem
mainWriterLoop h sem = mainRWLoop stdin h sem

{--
mainReaderLoop h sem = fix $ \loop -> do { 
  yield;
  ready <- hWaitForInput h 100; 
  when ready $
   do { 
      cont <- hGetLine h; 
      when (cont == "__quit__") $ do { void $ tryTakeMVar sem; putMVar sem (); };
      putStrLn cont; 
      hFlush stdout; } 
  cont <- isEmptyMVar sem;
  if cont then loop else putStrLn "Server closed connection."}

mainWriterLoop h sem = fix $ \loop -> do {  
    yield;
    ready <- hWaitForInput stdin (100); 
    when ready $ 
        do { line <- hGetLine stdin;
             when (line == "quit") $
             hPutStrLn h line; }
      else do { cont <- isEmptyMVar sem; when cont loop;};
      }
--}
{--
mainWriterLoop h = do
  input <- hReady stdin
  if input 
    then 
      do {
        resp <- getLine;
        if (resp !! 0) /= 'q' 
          then do { hPutStr h (resp++['\n']); mainLoop h;}
          else do { hPutStr h "quit\n"; putStr "Bye then.\n"; } }
    else do { hWaitForInput stdin 1000; mainLoop h; }
--}

printUsage name = putStr (name ++ " { chat | game } <hostname> <port>\n")
