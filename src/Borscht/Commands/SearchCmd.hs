{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadComprehensions #-}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use <$>" -}

--------------------------------------------------------------------------------

module Borscht.Commands.SearchCmd where

-- aeson
import Data.Aeson
import qualified Data.Aeson.Types as AT

-- control
import Control.Exception.Lifted (handle)
import Control.Applicative (liftA2, empty, (<|>))

-- monads
import Control.Monad ((>=>))
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Extra (concatMapM)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Except (
      ExceptT, runExceptT, throwError
  )

-- data
import Data.List (partition)
import Data.Maybe (fromMaybe)
import Data.Foldable (asum)

-- requests
import Network.HTTP.Req(HttpException(..))

-- strings
import Data.Text (Text)
import qualified Data.Text as T

-- pretty-simple
import Text.Pretty.Simple (pPrint)

-- time
import qualified Data.Time.Clock as Time (NominalDiffTime)
import qualified Data.Time.Format as Time (parseTimeM, defaultTimeLocale)

-- borscht
import Borscht.App (App(..), prepareCtx)
import Borscht.Util.Normalize (mkNormalizedText)
import Borscht.Autotag.Candidates (TrackCandidate(..), ArtistCandidate(..))

-- borscht discogs
import qualified Borscht.Req.Discogs      as Dgs
import qualified Borscht.Req.Discogs.JSON as Dgs
import Borscht.Req.Discogs
    ( SearchOpts, requestDiscogsSearch, requestDiscogsRelease, requestDiscogsMaster )
import Borscht.Req.Discogs.JSON (
    DiscogsRelease(..),
    DiscogsTrack(..),
    DiscogsSearchResults(searchResults),
    DiscogsSearchResult(resultId),
    DiscogsReleaseArtist(..),
    DiscogsMasterRelease(..)
  )

-- borscht musicbrainz
-- import Borscht.Req.MusicBrainz as MBZ (searchRecording)

--------------------------------------------------------------------------------

decodeValue :: FromJSON a => Value -> Maybe a
decodeValue = AT.parseMaybe parseJSON

decodeEither :: FromJSON a => Value -> Either String a
decodeEither = AT.parseEither parseJSON

printExceptT :: Show a => ExceptT a IO b -> IO ()
printExceptT p = runExceptT p >>= \case
                    Left e  -> print e
                    Right _ -> pure ()

-- redirect runtime exceptions to ExceptT
-- https://stackoverflow.com/a/26392842/1444650
intercept
  :: ExceptT String IO a
  -> ExceptT String IO a
intercept = handle handler
  where handler :: HttpException -> ExceptT String IO a
        handler = throwError . show

--------------------------------------------------------------------------------

-- example of how to use rate limiting
-- rateLimitExample :: App ()
-- rateLimitExample = do
--     rateGuard <- asks rateGuard

--     liftIO $ do
--         rateGuard >> print "hello"
--         rateGuard >> print "world"

--------------------------------------------------------------------------------

runSearchCmd :: SearchOpts -> IO ()
runSearchCmd opts
    = either print return
    =<< runExceptT (prepareSearch opts)

prepareSearch :: SearchOpts -> ExceptT String IO ()
prepareSearch opts = runReaderT (runApp $ runSearch opts) =<< prepareCtx

------------------------------------------------------------

runSearch :: SearchOpts -> App ()
runSearch query = do
    -- search MusicBrainz for releases
    --mbz1 <- MBZ.searchRecording searchOptions
    -- search discogs for a list of matching master releases
    results <- searchResults <$> requestDiscogsSearch query
    mapM_ pPrint results
    return ()

    -- -- query discogs api for details about each individual release
    -- releases <- mapM (requestDiscogsRelease . resultId) results
    -- -- rank candidates by ngram cosine distance to query title
    -- -- TODO normalize query fields
    -- let queryTitle = searchTitle query
    --     tracks     = releases >>= releaseTracks
    --     scored     = map (score &&& id) tracks
    --     candidates = sortOn (negate . fst) scored
    --     score      = (distanceCosine 3 queryTitle) . (normalized . trackTitle)

    -- liftIO $ do
    --     -- configure buffering (https://stackoverflow.com/a/10196382/1444650)
    --     hSetBuffering stdout LineBuffering -- or NoBuffering
    --     -- present the top five candidates
    --     mapM_ offerCandidate (take 5 candidates)
    --     mapM_ print candidates

    -- return ()


-- converts timestamp like "4:16" to 256 seconds
-- NominalDiffTime measures duration, ignoring leap seconds, and as a `RealFrac`
-- instance, behaves like its value in seconds upon conversions like `floor`
parseDuration :: Text -> Maybe Integer
parseDuration t = floor <$> (Time.parseTimeM True Time.defaultTimeLocale "%m:%S" (T.unpack t) :: Maybe Time.NominalDiffTime)

-- combines information from the track and its corresponding release 
candidateTrackFromDiscogs :: DiscogsRelease -> DiscogsTrack -> TrackCandidate
candidateTrackFromDiscogs r t = TrackCandidate {
        tcReleaseTitle = Just $ mkNormalizedText (releaseTitle r),
        tcDgsReleaseId = Just (releaseId r),
        tcMbzReleaseId = Nothing,
        tcGenres       = releaseGenres r,
        tcTitle        = mkNormalizedText (trackTitle t),
        tcArtists      = map candidateArtistFromDiscogs (releaseArtists r),
        tcYear         = releaseYear r,
        tcCountry      = releaseCountry r,
        tcDuration     = parseDuration (trackDuration t)
    }

candidateArtistFromDiscogs :: DiscogsReleaseArtist -> ArtistCandidate
candidateArtistFromDiscogs artist = ArtistCandidate {
        acId   = releaseArtistId artist,
        acName = mkNormalizedText (releaseArtistName artist)
    }

--------------------------------------------------------------------------------

-- this function is meant to be called by the autotagger
queryDiscogs :: SearchOpts -> App [DiscogsTrack]
queryDiscogs query = do
    -- search discogs for a list of matching releases (albums)
    results <- searchResults <$> requestDiscogsSearch query
    -- query discogs api for details about each individual release
    releases <- mapM (requestDiscogsRelease . resultId) results
    -- rank candidates by ngram cosine distance to query title
    return (releases >>= releaseTracks)

processRelease :: DiscogsRelease -> App [TrackCandidate]
processRelease r = do
    return $ map (candidateTrackFromDiscogs r) (releaseTracks r)

nonEmpty :: [a] -> Maybe [a]
nonEmpty [] = Nothing
nonEmpty as = Just as

-- Lazily-evaluated monoid instance, for trying a sequence of fallback actions.
-- Initially tried a @(First a)@, but the semigroup operation (<>) is not lazy.
-- The below works because the Alternative instance for the MaybeT monad transformer
-- returns the first successful result, without executing the remaining actions.
-- (thanks to https://stackoverflow.com/a/47126169/1444650)
newtype FirstT m a = FirstT (MaybeT m a)

firstT :: m (Maybe a) -> FirstT m a
firstT tma = FirstT (MaybeT tma)

getFirstT :: FirstT m a -> m (Maybe a)
getFirstT (FirstT (MaybeT tma)) = tma

instance Monad m => Semigroup (FirstT m a) where
    FirstT m1 <> FirstT m2 = FirstT $ m1 <|> m2

instance Monad m => Monoid (FirstT m a) where
    mempty = FirstT empty

-- request release info for a given search result
resultToRelease :: DiscogsSearchResult -> App DiscogsRelease
resultToRelease result = do
    case Dgs.resultType result of
        Dgs.ResultRelease -> (requestDiscogsRelease . resultId) result
        Dgs.ResultMaster  -> masterCanonicalRelease result

-- extract the canonical release for a "master" album
-- TODO: (2021-06-27) this is a partial function, only accepting masters!
-- illegal to call on releases (see the comment on Discogs.JSON for details)
masterCanonicalRelease :: DiscogsSearchResult -> App DiscogsRelease
masterCanonicalRelease
    =   (requestDiscogsMaster . resultId)
    >=> (requestDiscogsRelease . masterMainRelease)

queryDiscogsMasters :: SearchOpts -> App [TrackCandidate]
queryDiscogsMasters query = do
    -- search discogs for a list of matching tracks
    results <- searchResults <$> requestDiscogsSearch query

    -- a "master" represents an abstract "album" with potentially many "releases"
    -- we prefer "master" metadata when present, but fall back to orphan "releases"
    let (foundMasters, foundReleases) = partition ((== Dgs.ResultMaster) . Dgs.resultType) results

    liftIO $ do
        putStrLn $ "releases found: " ++ show (length foundMasters)
        putStrLn $ "masters found: " ++ show (length foundReleases)

    -- three ways to fetch a list of releases, in order of preference
    -- the later ones will be lazily evaluated if earlier ones fail
    let actions = map (firstT . fmap nonEmpty) [
           -- method 1: use the canonical release for each found master
           mapM masterCanonicalRelease foundMasters,
           -- method 2: use all the releases returned by the search query
           mapM (requestDiscogsRelease . resultId) foundReleases,
           -- method 3: search for the artist first, then attempt to match the title
           --      then search for the track first, then attempt to match the artist
           [ r1 ++ r2 | r1 <- artistThenTrack query , r2 <- trackThenArtist query ]
         ];

    -- use the result of the first successful action
    releases <- getFirstT $ foldl (<>) mempty actions

    -- 
    liftIO $ case releases of
        Nothing -> putStrLn "found nothing!!"
        _       -> putStrLn $ "found " ++ show (length releases) ++ " something!"

    concatMapM processRelease (fromMaybe [] releases)

-- search for the artist first, then attempt to match the title
artistThenTrack :: SearchOpts -> App [DiscogsRelease]
artistThenTrack query = do
    liftIO $ putStrLn "Searching for all releases by (artist)."
    
    -- query discogs for releases matching this artist
    artistResults <- case Dgs.searchArtist query of
        Nothing     -> return []
        Just artist -> do let params = (Dgs.matchArtist artist) <> (Dgs.matchType "release")
                          searchResults <$> requestDiscogsSearch params

    -- return all tracks belonging to top 5 releases matching this artist name
    mapM (requestDiscogsRelease . resultId) (take 5 artistResults)

-- search for the artist first, then attempt to match the title
trackThenArtist :: SearchOpts -> App [DiscogsRelease]
trackThenArtist query = do
    liftIO $ putStrLn "Searching for all releases by (title)."
    
    artistResults <- case Dgs.searchTitle query of
        Nothing     -> return []
        Just title -> searchResults <$> requestDiscogsSearch (Dgs.matchTitle title)

    -- return all tracks belonging to top 5 releases matching this artist name
    mapM resultToRelease (take 5 artistResults)