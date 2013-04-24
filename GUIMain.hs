module GUIMain where

import qualified Client
import qualified Server
import MyIOLib

import System.Environment (getProgName, getArgs)
import Graphics.UI.Gtk hiding (disconnect)
import Graphics.UI.Gtk.Glade
import Control.Monad (void, when)
import Control.Concurrent
import Control.Concurrent.MVar
import Network

data GUI = GUI { window :: Window, text :: TextView, entry :: Entry, sem :: MVar Int}

main progName args guipath = do
  initGUI
  timeoutAddFull (yield >> return True)
                    priorityDefaultIdle 100
  gui <- loadGlade guipath
  connectGUI gui
  widgetShowAll ( window gui )
  if (length args) < 3 then do { printUsage progName; error "More args please" } else do 
    let stype = args !! 0
    let hout = CConn (addTextToBuffer (text gui)) (addTextToBuffer (text gui) "Bye then..")
    when ( stype == "client") $ if (length args) < 4 
      then printUsage progName
      else clientPart gui hout
    when ( stype == "server") $ serverPart gui hout
  mainGUI
  where
    clientPart gui hout = do
      putStrLn "GUI client starting.."
      let hostname = (args !! 2)
      let port = (PortNumber $ toEnum (read (args !! 3) :: Int))
      let ctype = args !! 1
      if ( ctype == "chat") 
        then clientChat hout hostname port gui
        else clientGame hout hostname port gui
    serverPart gui hout = do
      putStrLn "GUI server starting.."
      let port = (PortNumber $ toEnum (read (args !! 2) :: Int))
      let ctype = args !! 1
      if ( ctype == "chat") 
        then Server.mainChat hout port
        else Server.mainGame hout port

clientChat hout hostname port gui = do
  hin <- Client.mainChat hout hostname port
  connectClientChatGUI hin gui

connectClientChatGUI f gui = do
  void $ tryTakeMVar (sem gui)
  putMVar (sem gui) 2
  void $ afterEntryActivate (entry gui) $ 
    do { text <- entryGetText (entry gui); cdoReact f $ text; clearEntryIf (entry gui) (sem gui);}

clientGame hout hostname port gui = do
  hin <- Client.mainGame hout hostname port
  connectClientGameGUI hin gui

connectClientGameGUI = connectClientChatGUI

printUsage pn = do
  putStrLn $ "Usage: " ++ pn ++ " <guifile>"

connectGUI g = do
  connectEntryText (entry g) (text g) (sem g)
  onDestroy (window g) mainQuit
  where 
    connectEntryText entry textview sem = do {
      buffer <- textViewGetBuffer textview;
      endIter <- textBufferGetEndIter buffer;
      void $ textBufferCreateMark buffer (Just "end") endIter False; 
      void $ afterEntryActivate entry (addTextFromEntry entry textview sem); 
      }


addTextFromEntry entry textview sem = do
  text <- entryGetText entry
  addTextToBuffer textview text
  clearEntryIf entry sem
 
clearEntryIf entry sem = do
  doClear <- takeMVar sem
  if (doClear == 0 || doClear == 1) 
    then do {
      entrySetText entry "";
      when (doClear == 1) $ putMVar sem 2;
      when (doClear == 0) $ putMVar sem 0; }
    else putMVar sem 1;
  
addTextToBuffer textview text = do
  buffer <- textViewGetBuffer textview
  Just endMark <- textBufferGetMark buffer "end" 
  endIter <- textBufferGetEndIter buffer
  when (text /= "") $ textBufferInsert buffer endIter (text++"\n")
  textViewScrollMarkOnscreen textview endMark

loadGlade path = do
  Just xml <- xmlNew path
  mw <- xmlGetWidget xml castToWindow "window1"
  [mwEntry1] <-
    mapM (xmlGetWidget xml castToEntry) ["entry1"]
  [mwTextView1] <-
    mapM (xmlGetWidget xml castToTextView) ["textview1"]
  sem <- newMVar 0
  return $ GUI mw mwTextView1 mwEntry1 sem
