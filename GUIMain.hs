
module GUIMain where

import System.Environment (getProgName, getArgs)
import Graphics.UI.Gtk hiding (disconnect)
import Graphics.UI.Gtk.Glade
import Control.Monad (void, when)
import Control.Concurrent

data GUI = GUI { window :: Window, text :: TextView, entry :: Entry}

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
        when ( ctype == "chat") $ Client.mainChat hout hostname port;
        when ( ctype == "game") $ Client.mainGame hout hostname port; }; }
    when ( stype == "server") $ do {
      let port = (PortNumber $ toEnum (read (args !! 2) :: Int))
      when ( ctype == "chat") $ Server.mainChat hout port;
      when ( ctype == "game") $ Server.mainGame hout port; }
  mainGUI

printUsage pn = do
  putStrLn $ "Usage: " ++ pn ++ " <guifile>"

connectGUI g = do
  connectEntryText (entry g) (text g)
  onDestroy (window g) mainQuit
  where 
    connectEntryText entry textview = do {
      buffer <- textViewGetBuffer textview;
      endIter <- textBufferGetEndIter buffer;
      void $ textBufferCreateMark buffer (Just "end") endIter False; 
      void $ afterEntryActivate entry (addTextToBuffer entry textview); 
      }


addTextFromEntry entry textview = do
  text <- entryGetText entry
  entrySetText entry ""
  addTextToBuffer textview text
 
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
  return $ GUI mw mwTextView1 mwEntry1
