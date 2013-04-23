import qualified GUIMain
import qualified CLIMain
import Network.Socket (withSocketsDo)
import System.Environment (getArgs)

main = withSocketsDo $ do
  args <- getArgs
  if (head args) == "--cli" 
    then CLIMain.main (tail args)
    else GUIMain args
