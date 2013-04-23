
module GUIMain where

import System.Environment (getProgName, getArgs)
import Graphics.UI.Gtk hiding (disconnect)
import Graphics.UI.Gtk.Glade
import Control.Monad (void, when)
import Control.Concurrent

data GUI = GUI { window :: Window, readerText :: TextView, readerEntry :: Entry, writerText:: TextView, writerEntry :: Entry}

main = do
  args <- getArgs
  if length args < 1 then do { pName <- getProgName; printUsage pName; } else do
    initGUI
    timeoutAddFull (yield >> return True)
                    priorityDefaultIdle 100
    gui <- loadGlade (args !! 0)
    connectGUI gui
    widgetShowAll ( window gui )
    mainGUI

printUsage pn = do
  putStrLn $ "Usage: " ++ pn ++ " <guifile>"

connectGUI g = do
  connectEntryText (writerEntry g) (writerText g)
  connectEntryText (readerEntry g) (readerText g)
  onDestroy (window g) mainQuit
  where 
    connectEntryText entry textview = do {
      buffer <- textViewGetBuffer textview;
      endIter <- textBufferGetEndIter buffer;
      void $ textBufferCreateMark buffer (Just "end") endIter False; 
      void $ afterEntryActivate entry (addTextToBuffer entry textview); 
      }
  
addTextToBuffer entry textview = do
  buffer <- textViewGetBuffer textview
  Just endMark <- textBufferGetMark buffer "end" 
  text <- entryGetText entry
  entrySetText entry "" 
  endIter <- textBufferGetEndIter buffer
  when (text /= "") $ textBufferInsert buffer endIter (text++"\n")
  textViewScrollMarkOnscreen textview endMark

loadGlade path = do
  Just xml <- xmlNew path
  mw <- xmlGetWidget xml castToWindow "window1"
  [mwEntry1, mwEntry2] <-
    mapM (xmlGetWidget xml castToEntry) ["entry1", "entry2"]
  [mwTextView1, mwTextView2] <-
    mapM (xmlGetWidget xml castToTextView) ["textview1", "textview2"]
  return $ GUI mw mwTextView1 mwEntry1 mwTextView2 mwEntry2
