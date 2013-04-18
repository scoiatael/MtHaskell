-- with apologies for the lack of comments :)
 
import Network
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Fix (fix)
import System.Environment (getArgs, getProgName)
 
type Msg = (Int, String)
 
main :: IO ()
main = do
    chan <- newChan
    args <- getArgs
    when ((length args) < 1) $ do { printUsage; return (); }
{--
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
    listen sock 2
--}
    sock <- listenOn (PortNumber (toEnum (read (args !! 0) :: Int)))
    forkIO $ fix $ \loop -> do
        (_, msg) <- readChan chan
        loop
    mainLoop sock chan 0
 
printUsage = do { prog <- getProgName; putStr ("Usage: " ++ prog ++ " <port>\n"); }

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan nr = do
    conn <- accept sock
    forkIO (runConn conn chan nr)
    mainLoop sock chan $! nr+1
 
runConn :: (Handle, HostName, PortNumber) -> Chan Msg -> Int -> IO ()
runConn (hdl, _, _) chan nr = do
    let broadcast msg = do { writeChan chan (nr, msg); putStr (msg++"\n"); }
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
