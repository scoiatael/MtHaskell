module MyIOLib where

import System.IO
import Control.Monad
import Control.Exception

type InputF = IO Input
type OutputF = Input -> IO ()
type PromptF = Input -> IO Input

type Input = String
data ClientConnection = CConn { csend :: OutputF, cquit :: IO (), cget :: InputF, cprompt :: PromptF}
data ServerConnection = SConn { ssend :: OutputF, squit :: IO (), sget :: InputF }

type Reaction = Input -> IO ()
newtype ClientReaction = CR { cdoReact :: Reaction}
newtype ServerReaction = SR { sdoReact :: Reaction} 

handleToInputF h = do
  hGetLine h
{--do {
  ready <- hWaitForInput h 100;
  if ready then do { str <- hGetLine h; return $ Just str }
    else do { return Nothing; }; } --}
 
handleToPromptF h str = do
  hPutStrLn h str
  hFlush h
  hGetLine h 

handleToOutputF h str = do {
  hPutStrLn h str;
  hFlush h; }

handleToServerConnection h = SConn (handleToOutputF h) (endConnection h) (handleToInputF h)

endConnection hdl = do
  putStrLn "Quitting connection..."
  isOpen <- hIsOpen hdl
  when isOpen $ do { hPutStrLn hdl "__quit__"; putStrLn "sent goodbyes..";  hClose hdl} `catch` exceptionHandler

exceptionHandler :: SomeException -> IO ()
exceptionHandler e = do {putStr "Error ignored: "; print $ toException e;}
