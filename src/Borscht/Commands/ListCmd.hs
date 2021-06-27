{-# LANGUAGE OverloadedStrings #-}

module Borscht.Commands.ListCmd where

-- system
import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath (takeExtension, (</>))
import qualified System.Directory as D

-- data
import Data.Sort (sortOn)
import Data.Default
import Data.Tuple.Extra (
    (&&&) -- (a -> b) -> (a -> c) -> a -> (b,c)
  )

-- control
import Control.Monad.Extra (concatMapM, partitionM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import UnliftIO (MonadUnliftIO)
import Control.Monad.Except (
      MonadError, liftEither,
      ExceptT(ExceptT), runExceptT,
      throwError, catchError
  )
import Control.Monad.Reader (
      MonadReader, ReaderT, runReaderT,
      ask, asks
  )

-- pretty-simple
import Text.Pretty.Simple (pPrint)


-- text
import Formatting ((%))
import qualified Formatting as Fmt
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.ICU.Regex as RX

-- audio
import qualified Sound.HTagLib as HTL

-- database
import Database.Persist.Sqlite

-- project imports
import Borscht.App (App(..), prepareCtx)
import Borscht.Db.Queries ( queryArtistName )
import qualified Borscht.Db.Model as Db
import Borscht.Commands (ListOpts(ListOpts))
import Borscht.Commands.SearchCmd (queryDiscogsMasters)
import Borscht.Req.Discogs
import Borscht.Autotag.Candidates (TrackCandidate(..), ArtistCandidate(..))
import Borscht.Util.Fuzzy (distanceCosine)
import Borscht.Util.Normalize (normalized, NormalizedText(..))

------------------------------------------------------------

-- ./{ARTIST}/{ALBUM_TITLE}/{SONG_TITLE} - {YEAR}.*

isMusic :: FilePath -> Bool
isMusic path = (takeExtension path) `elem` [".m4a", ".mp3"]

runListCmd :: ListOpts -> IO ()
runListCmd opts =
    either print return
    =<< runExceptT (prepareList opts)

prepareList :: ListOpts -> ExceptT String IO ()
prepareList opts = do
    ctx <- prepareCtx
    -- run search
    runReaderT (runApp $ runListDir opts) ctx

runListDir :: ListOpts -> App ()
runListDir (ListOpts dir) = do
    musicDir <- liftIO $ D.makeAbsolute (Text.unpack dir)
    musicFiles <- liftIO $ listRecursive isMusic musicDir

    -- ensure database exists, performing setup if necessary
    -- TODO (2021-06-23) shouldn't rely on persistent's auto-migration
    -- TODO (2021-06-23) use persistent-mtl to make setup/queries easier
    liftIO $ Db.migrate "testit.db"

    -- runSqlite "testit.db" $ do
    --     --artistId <- queryArtistName "Mazouni"
    --     artists <- mapM queryFile musicFiles
    --     liftIO $ putStr "hello"

    artists <- mapM autotagFile musicFiles
    pure ()

    -- add all music files to the database


    --regex <- RX.regex [] "/([^/\\])/()/().()"

    -- runSqlite ":memory:" $ do
    --     liftIO $ putStrLn "connecting"

    -- mapM_ displayFile musicFiles

------------------------------------------------------------

autotagFile :: FilePath -> App ()
autotagFile path = do
    liftIO $ putStrLn $ "\n\nIMPORTING: " ++ path
    -- parse file metadata
    track <- HTL.getTags path audioTrackGetter
    liftIO $ pPrint track

    -- query discogs api for matching tracks
    let title  = HTL.unTitle $ atTitle track
    let artist = HTL.unTitle $ atTitle track
    let query = matchTitle title <> matchArtist artist

    queryDiscogsMasters query >>= \case
        [] -> handleZeroCandidates
        cs -> handleSomeCandidates title cs

handleZeroCandidates :: App ()
handleZeroCandidates = do
    liftIO $ putStrLn "No candidates found."

handleSomeCandidates :: Text -> [TrackCandidate] -> App ()
handleSomeCandidates queryTitle candidates = do
    let normTitle = normalized queryTitle
        score     = (distanceCosine 3 normTitle) . (textNormal . tcTitle)
        scored    = map (score &&& id) candidates
        ranked    = sortOn (negate . fst) scored
    
    liftIO $ do
        let nc = length candidates
            ns = 5
        Fmt.fprint ("Found " % Fmt.int % " candidates, showing top " % Fmt.int % ":\n") (nc) (min nc ns)
        mapM_ print (take ns ranked)

------------------------------------------------------------

queryFile :: (MonadUnliftIO m) => FilePath -> Db.SqliteAction m (Key Db.Artist)
queryFile path = do
    -- parse file metadata
    track <- HTL.getTags path audioTrackGetter
    -- fetch or insert artist information
    artistId <- queryArtistName (HTL.unArtist $ atArtist track)
    liftIO $ putStrLn path
    --liftIO $ putStr "artist: " >> print artistId
    -- insert track information
    insert $ Db.Track (HTL.unTitle $ atTitle track) artistId 
    pure artistId

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