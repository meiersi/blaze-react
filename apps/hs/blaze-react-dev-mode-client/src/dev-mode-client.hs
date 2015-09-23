
import qualified Blaze.Development.Client as Client


type WindowState act = act

runApp'
    :: (Show act)
    => (st -> WindowState act)
    -> App st act ((act -> IO ()) -> IO ())
    -> IO ()
runApp'

