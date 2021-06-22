{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use <$>" -}

--------------------------------------------------------------------------------

module Borscht.Commands.SearchCmd where

-- system
import System.Environment (lookupEnv)

-- aeson
import Data.Aeson
import qualified Data.Aeson.Types as AT

-- control
import Control.Monad (void, filterM, guard, join, ap)
import Control.Exception.Lifted (try, handle)

-- monad transformers
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (
      MonadReader, ReaderT, runReaderT,
      ask, asks
    )
import Control.Monad.Except (
      MonadError, liftEither,
      ExceptT(ExceptT), runExceptT,
      throwError, catchError
    )

-- data
import Data.Bifunctor (first, second)

-- requests
import qualified Network.HTTP.Req as Req
import Network.HTTP.Req(
      (/:), (=:),
      defaultHttpConfig,
      https, GET(GET),
      Req, req, runReq,
      JsonResponse, jsonResponse,
      responseBody,
      NoReqBody(NoReqBody),
      Option,
      HttpException(VanillaHttpException)
    )

-- strings
import Data.Text (Text, intercalate)
import qualified Data.Text as T
import qualified Data.Text.Lazy as T (toStrict)
import qualified Data.Text.Lazy.Builder as TB (toLazyText)
import qualified Data.Text.Lazy.Builder.Int as TB (decimal)
import qualified Data.Text.IO as T
import qualified Data.ByteString as BS  (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B

-- fuzzy string matching
import qualified Data.FuzzySet as FZ

-- rate limit
import Data.Time.Units ( Second )
import Borscht.Util.RateLimit (rateLimitInvocation)

-- lens
import Control.Lens.Tuple (_1, _2)
import Control.Lens.Operators ((.~))

-- borscht
import Borscht.App (App(..), Ctx(..))
import Borscht.Commands (SearchOpts, searchTitle, searchArtist)
import Borscht.Util.Functions (fork, dupe, mapFst, mapSnd)
import Borscht.Util.Fuzzy (distanceCosine, normalized)

-- borscht discogs
import Borscht.Req.Discogs
    ( requestDiscogsSearch, requestDiscogsRelease )
import Borscht.Req.Discogs.JSON (
    DiscogsRelease(..),
    DiscogsTrack(..),
    DiscogsSearchResults(searchResults),
    DiscogsSearchResult(resultTitle, resultYear, resultType, resultId)
  )

-- borscht musicbrainz
import Borscht.Req.MusicBrainz as MBZ (searchRecording)

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

throwMaybe :: (MonadError b m) => b -> Maybe a -> m a
throwMaybe e Nothing  = throwError e
throwMaybe _ (Just x) = pure x

runSearchCmd :: SearchOpts -> IO ()
runSearchCmd opts
    = either print return
    =<< runExceptT (prepareSearch opts)

getEnv :: (MonadIO m, MonadError String m) => String -> m String
getEnv name
    = liftIO (lookupEnv name)
    >>= throwMaybe ("environment variable" ++ name ++ " not found")

prepareSearch :: SearchOpts -> ExceptT String IO ()
prepareSearch opts = do
    -- prepare api key
    authKey <- getEnv "DISCOGS_KEY"
    -- prepare rate limiter
    rateGuard <- liftIO $ rateLimitInvocation (1 :: Second) (pure :: () -> IO ())
    -- prepare request context
    let ctx = Ctx {
        ctxDiscogsAuth   = authKey,
        ctxSearchOptions = opts,
        ctxRateGuard     = rateGuard ()
    }
    -- run search
    runReaderT (runApp runSearch) ctx

---------------------

runSearch :: App ()
runSearch = do
    searchOptions <- asks ctxSearchOptions

    -- search for releases
    --mbz1 <- MBZ.searchRecording searchOptions
    req1 <- requestDiscogsSearch searchOptions

    -- we are looking for specific "release"s, not general "master" listings
    let results = filter ((== "release") . resultType) (searchResults req1)

    -- query discogs api for details about each individual release
    releases <- mapM (requestDiscogsRelease . resultId) results
    let tracks = releases >>= releaseTracks

    let titles = map (normalized . trackTitle) tracks
    -- let fuzzy = FZ.fromList titles
    let query = searchTitle searchOptions

    liftIO $ print ("query: " ++ (T.unpack query))
    liftIO $ mapM_ (\t -> print (t, distanceCosine 3 query t)) titles

    return ()

--------------------------------------------------------------------------------

displaySearchResults :: DiscogsSearchResults -> IO ()
displaySearchResults x = mapM_ displaySearchResult (searchResults x)

displaySearchResult :: DiscogsSearchResult -> IO ()
displaySearchResult x = do
    T.putStrLn $ intercalate " -- " [resultTitle x, resultYear x]

-- ////////////////////////////////////////////////////////////////////////// --

handleResponse
    :: (MonadError String m, FromJSON a)
    => JsonResponse Value -> m a
handleResponse resp = case decodeValue (responseBody resp) of
    Nothing -> throwError "failed to decode response body"
    Just r  -> return r