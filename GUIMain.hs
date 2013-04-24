
module GUIMain where

import System.Environment (getProgName, getArgs)
import Graphics.UI.Gtk hiding (disconnect)
import Graphics.UI.Gtk.Glade
import Control.Monad (void, when)
import Control.Concurrent
import Control.Concurrent.MVar

data GUI = GUI { window :: Window, text :: TextView, entry :: Entry, sem :: MVar Int}

main progname args guipath = do
  initGUI
  gui <- loadGlade guipath
  connectGUI gui
  widgetShowAll ( window gui )
  if (length args) < 3 then printUsage progName else do 
    let ctype = args !! 1
    let stype = args !! 0
    let hout = addTextToBuffer (text gui)
    when ( stype == "client") $ do {
      let hostname = (args !! 2)
      if (length args) < 4 then printUsage progName else do {
        let port = (PortNumber $ toEnum (read (args !! 3) :: Int));
        if ( ctype == "chat") then clientChat hout hostname port gui;
          else Client.mainGame hout hostname port gui; }; }
    when ( stype == "server") $ do {
      let port = (PortNumber $ toEnum (read (args !! 2) :: Int))
      if ( ctype == "chat") then Server.mainChat hout port gui;
        else Server.mainGame hout port gui; }
  mainGUI

clientChat hout hostname port = do
  hin <- Client.mainChat hout hostname port
  connectClientChatGUI hin
  where
    connectClientChatGUI f = void $ afterEntryActivate (entry gui) do {f; clearEntryIf (entry gui) (sem gui)}

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
      void $ afterEntryActivate entry (addTextToBuffer entry textview sem); 
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
