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

data GUI = GUI { window :: Window, text :: TextView, entry :: Entry, sem :: MVar Int, listenerNr :: MVar Int }

main progName args guipath = do
  initGUI
  timeoutAddFull (yield >> return True)
                    priorityDefaultIdle 100
  gui <- loadGlade guipath
  connectGUI gui
  widgetShowAll ( window gui )
  if (length args) < 3 then do { printUsage progName; error "More args please" } else do 
    let stype = args !! 0
    let hout = CConn (addTextToBuffer (text gui)) (addTextToBuffer (text gui) "Bye then..") (waitForInputOnEntry gui)
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

waitForInputOnEntry gui = do
  str <- newEmptyMVar
  lisNr <- takeMVar (listenerNr gui)
  putMVar (listenerNr gui) (lisNr+1)
  connid <- afterEntryActivate (entry gui) $ do { 
    line <- entryGetText (entry gui); 
    putMVar str line; }
  line <- takeMVar str
  signalDisconnect connid
  lisNr' <- takeMVar (listenerNr gui)
  putMVar (listenerNr gui) (lisNr'-1)
  return line

clientChat hout hostname port gui = do
  hin <- Client.mainChat hout hostname port
  connectClientChatGUI hin gui

connectClientChatGUI f gui = do
  void $ tryTakeMVar (listenerNr gui)
  putMVar (listenerNr gui) 1
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
 
clearEntryIf entry sem sem2 = do
  doClear <- takeMVar sem
  lisNr <- takeMVar sem2
  if (doClear == lisNr) 
    then do {
      entrySetText entry "";
      putMVar sem 0; }
    else putMVar sem (doClear+1);
  putMVar sem2 listNr
  
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
  sem2 <- newMVar 0
  return $ GUI mw mwTextView1 mwEntry1 sem sem2
