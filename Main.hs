import qualified GUIMain
import qualified CLIMain
import Network.Socket (withSocketsDo)
import System.Environment (getArgs, getProgName)
import Paths_mth(getDataFileName)

main = withSocketsDo $ do
  args <- getArgs
  progName <- getProgName
  if ((length args) /= 0) && (head args) == "--cli" 
    then CLIMain.main progName (tail args)
    else do {
      gladepaths <- getDataFileName "mthresources.glade"; 
      GUIMain.main progName args gladepaths; }
