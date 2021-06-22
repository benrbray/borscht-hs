module Borscht.Commands.ListCmd where

-- system
import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath (takeExtension, (</>))
import qualified System.Directory as D

-- control
import Control.Monad.Extra (concatMapM, partitionM)

-- data

-- text
import qualified Data.Text as Text
import qualified Data.Text.ICU.Regex as RX

-- audio
import qualified Sound.HTagLib as HTL

-- project imports
import Borscht.Commands (ListOpts(ListOpts))

------------------------------------------------------------

-- ./{ARTIST}/{ALBUM_TITLE}/{SONG_TITLE} - {YEAR}.*

isMusic :: FilePath -> Bool
isMusic path = (takeExtension path) `elem` [".m4a", ".mp3"]

runListDir :: ListOpts -> IO ()
runListDir (ListOpts dir) = do
    musicDir <- D.makeAbsolute (Text.unpack dir)
    musicFiles <- listRecursive isMusic musicDir

    --regex <- RX.regex [] "/([^/\\])/()/().()"

    mapM_ displayFile musicFiles

displayFile :: FilePath -> IO ()
displayFile path = do
    putStrLn "-----"
    putStrLn path
    track <- HTL.getTags path audioTrackGetter
    print track

------------------------------------------------------------

-- | A version of System.FilePath.listDirectory which returns
-- | absolute paths. Expects an absolute directory path.
listDirectoryAbs :: FilePath -> IO [FilePath]
listDirectoryAbs dir = map (dir </>) <$> listDirectory dir

-- | recursively search the directory for files matching the predicate 
-- TODO how does this function handle symlinks?
listRecursive :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
listRecursive keep root = do
    (dirs, files) <- partitionM doesDirectoryExist =<< listDirectoryAbs root
    subresults <- concatMapM (listRecursive keep) dirs
    return $ (filter keep files) ++ subresults

------------------------------------------------------------

data AudioTrack = AudioTrack
  { atTitle   :: HTL.Title
  , atArtist  :: HTL.Artist
  , atAlbum   :: HTL.Album
  , atComment :: HTL.Comment
  , atGenre   :: HTL.Genre
  , atYear    :: Maybe HTL.Year
  , atTrack   :: Maybe HTL.TrackNumber }
  deriving Show

audioTrackGetter :: HTL.TagGetter AudioTrack
audioTrackGetter = AudioTrack
  <$> HTL.titleGetter
  <*> HTL.artistGetter
  <*> HTL.albumGetter
  <*> HTL.commentGetter
  <*> HTL.genreGetter
  <*> HTL.yearGetter
  <*> HTL.trackNumberGetter