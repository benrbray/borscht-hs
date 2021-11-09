{-# LANGUAGE OverloadedStrings #-}

module Borscht.Commands.ListCmd where

-- system
import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath (takeExtension, (</>))
import qualified System.Directory as D

-- data
import Data.Sort (sortOn)
import Data.Default
import Data.Function ((&))
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

-- formatting / printing
import Text.Pretty.Simple (pPrint)
import qualified Byline as BL
import qualified System.Console.ANSI as ANSI
import Formatting ((%))
import qualified Formatting as Fmt
import qualified Formatting.Combinators as Fmt

-- text
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)
import qualified Text.Printf as TP
import qualified Data.Text.Lazy as LT
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
    musicDir <- liftIO $ D.makeAbsolute (T.unpack dir)
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
    let artist = HTL.unArtist $ atArtist track
    let query = matchTitle title <> matchArtist artist

    queryDiscogsMasters query >>= \case
        [] -> handleZeroCandidates
        cs -> handleSomeCandidates title artist cs

handleZeroCandidates :: App ()
handleZeroCandidates = do
    liftIO $ putStrLn "No candidates found."

handleSomeCandidates :: Text -> Text -> [TrackCandidate] -> App ()
handleSomeCandidates queryTitle queryArtist candidates = do
    let normTitle    = normalized queryTitle
        normArtist   = normalized queryArtist
        scoreTitle   = (distanceCosine 3 normTitle) . (textNormal . tcTitle)
        scoreArtist  = (distanceCosine 3 normArtist) . textNormal . acName
        scoreArtists = foldr (max . scoreArtist) 0 . tcArtists
        score        = \c -> (scoreArtists c) * (scoreTitle c)
        scored       = filter ((>= 0.4) . fst) $ map (score &&& id) candidates
        ranked       = sortOn (negate . fst) scored
        nc           = length ranked
    
    if nc == 0
      then handleZeroCandidates
      else liftIO $ do
        let ns = 5
        Fmt.fprint ("Found " % Fmt.int % " candidates, showing top " % Fmt.int % ":\n") (nc) (min ns nc)
        let nats = [1,2..] :: [Int]
        let xxx  = zip nats ranked
        a <- BL.runBylineT $ do
            mapM_ sayCandidate (take ns xxx)
            askCandidateAction
        print a
        return ()

data CandidateAction
  = CndSelect Int
  | CndSkip
  | CndKeepOriginal
  deriving (Eq, Show)

askCandidateAction :: BL.MonadByline m => m CandidateAction
askCandidateAction = do
    let optcolor = BL.bold . (BL.fg BL.green)
        message = "select " <> ("#" & optcolor) <> ", "
               <> ("S" & optcolor) <> "kip"              <> ", "
               <> ("K" & optcolor) <> "eep original"     <> ", "
               <> ("E" & optcolor) <> "nter search"      <> ", "
               <> "enter " <> ("I" & optcolor) <> "d? "

    response <- BL.askLn message (Just "S")
    BL.sayLn $ BL.text ("your response was " <> response)
    let maybeIdx = readMaybe (T.unpack response)
    return $ case response of
        "S" -> CndSkip
        "K" -> CndKeepOriginal
        _   -> maybe CndSkip CndSelect maybeIdx


-- pads the label with extra characters so its length is at least n 
padLabel :: Int -> Char -> String -> String
padLabel n pad label = label ++ replicate (max 0 ((length label) - n)) pad

textMaybe :: String -> Maybe Text -> IO ()
textMaybe label m = stringMaybe label (T.unpack <$> m)

stringMaybe :: String -> Maybe String -> IO ()
stringMaybe label m = case m of
    Nothing -> pure ()
    Just s  -> putStrLn $ label ++ s

sayCandidate :: BL.MonadByline m => (Int, (Double, TrackCandidate)) -> m ()
sayCandidate (idx, (score,c)) = do
    let title   = (textOriginal . tcTitle $ c)
        artists = (T.intercalate " / " (textOriginal . acName <$> (tcArtists c)))
        album   = (textOriginal <$> (tcReleaseTitle c))
        genres  = (T.intercalate " | " (tcGenres c))

    let numberPart = BL.text $ Fmt.sformat (Fmt.int % ". ") idx
    let titlePart  = BL.text title & BL.fg (BL.vivid BL.blue) & BL.bold
    let artistPart = BL.text artists & BL.bold & BL.fg (BL.vivid BL.white)
    let scorePart  = BL.text (Fmt.sformat (" (" % (Fmt.fixed 0) % "%)") (score * 100)) & BL.fg BL.yellow
    BL.sayLn $ numberPart <> titlePart <> " -- " <> artistPart <> scorePart

    let color = BL.fg (BL.dull BL.white)
    let albumPart = BL.text $ Fmt.sformat (Fmt.optioned Fmt.stext) album
    BL.sayLn $ ("     album:  " & color) <> albumPart

    let yearPart = BL.text $ Fmt.sformat (Fmt.optioned Fmt.int) (tcYear c)
    BL.sayLn $ ("     year:   " & color) <> yearPart
    BL.sayLn $ ("     genres: " & color) <> (BL.text genres)

printCandidate :: (Int, (Double, TrackCandidate)) -> IO ()
printCandidate (idx, (score,c)) = do
    let title   = (textOriginal . tcTitle $ c)
        artists = T.unpack (T.intercalate " / " (textOriginal . acName <$> (tcArtists c)))
        album   = (textOriginal <$> (tcReleaseTitle c))

    -- let x = Fmt.format (Fmt.int % ". " % Fmt.string % " -- " % Fmt.string % " (" % (Fmt.fixed 0) % "%)\n")
    --     idx title artists (score*100)

    liftIO $ textMaybe   "     album:  " album
    liftIO $ stringMaybe "     year:   " (show <$> (tcYear c))
    liftIO $ putStrLn $  "     genres: " ++ T.unpack (T.intercalate " | " (tcGenres c))

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