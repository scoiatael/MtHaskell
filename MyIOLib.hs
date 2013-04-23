module MyIOLib where

import System.IO

type InputF = Int -> IO (Maybe String)
type OutputF = String -> IO ()

handleToInputF h time = do {
  ready <- hWaitForInput h time;
  if ready then do { str <- hGetLine h; return $ Just str }
    else do { return Nothing; }; }

handleToOutputF h str = do {
  hPutStrLn h str;
  hFlush h; }

