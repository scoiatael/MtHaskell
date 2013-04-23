import qualified GUIMain
import qualified CLIMain
import Network.Socket (withSocketsDo)
import System.Environment (getArgs)
import Paths_mth(getDataFileName)

main = withSocketsDo $ do
  args <- getArgs
  if ((length args) /= 0) && (head args) == "--cli" 
    then CLIMain.main (tail args)
    else do {
      gladepaths <- getDataFileName "mthresources.glade"; 
      GUIMain.main args gladepaths; }
