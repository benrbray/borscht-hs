module ListCmd where

-- system
import System.Directory (listDirectory)

-- project imports
import Commands (ListOpts(ListOpts))
import qualified Data.Text as Text

------------------------------------------------------------

runListDir :: ListOpts -> IO ()
runListDir (ListOpts dir) = do {
    x <- listDirectory (Text.unpack dir) ;
    print x ;
}
