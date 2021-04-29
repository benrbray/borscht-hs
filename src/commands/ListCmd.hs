module ListCmd where

-- system
import System.Directory (listDirectory)

-- project imports
import Commands (ListOpts(ListOpts))

------------------------------------------------------------

runListDir :: ListOpts -> IO ()
runListDir (ListOpts dir) = do {
    x <- listDirectory dir ;
    print x ;
}
