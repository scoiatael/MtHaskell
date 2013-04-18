import Network
import System.IO
import System.Environment (getArgs, getProgName)
import Control.Monad.Fix (fix)
import Control.Monad
import Control.Concurrent

main = withSocketsDo $ do
  args <- getArgs
  if (length args) < 2 then do { name <- getProgName; printUsage name;} else do 
    h <- connectTo (args !! 0) (PortNumber $ toEnum (read (args !! 1) :: Int))
    hSetBuffering h NoBuffering
    putStr "\n[q]uit to quit.\n"
    reader <- forkIO $ mainReaderLoop h
    mainWriterLoop h
    killThread reader
    hClose h

mainReaderLoop h = fix $ \loop -> do { ready <- hReady h; if ready then do { cont <- hGetLine h;  putStr (cont++['\n']); hFlush stdout; } else void $ hWaitForInput h 100; loop}
mainWriterLoop h = fix $ \loop -> 
  do { 
    ready <- hWaitForInput stdin (-1); 
    if ready 
      then 
        do { cont <- hGetLine stdin;
             hPutStr h (cont++['\n']);
             if ((cont !! 0) == 'q') then putStr "Bye then.\n" else loop; } 
      else putStr "Timed out.\n"; 
      }

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

printUsage name = putStr (name ++ "<hostname> <port>\n")
