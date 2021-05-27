{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use <$>" -}

--------------------------------------------------------------------------------

module SearchCmd where

-- system
import System.Environment (lookupEnv)

-- aeson
import Data.Aeson
import qualified Data.Aeson.Types as AT
import Data.Aeson.Encode.Pretty (encodePretty)

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
import Data.Char (chr)
import Data.Text (Text, intercalate)
import qualified Data.Text as T
import qualified Data.Text.Lazy as T (toStrict)
import qualified Data.Text.Lazy.Builder as TB (toLazyText)
import qualified Data.Text.Lazy.Builder.Int as TB (decimal)
import qualified Data.Text.IO as T
import qualified Data.ByteString as BS  (ByteString)
import qualified Data.ByteString.Char8 as C8 (pack)
import qualified Data.ByteString.Lazy.Char8 as B

-- rate limit
import Data.Time.Units ( Second )
import RateLimit (rateLimitInvocation)

-- project imports
import Commands (SearchOpts)
import Discogs (
    DiscogsRelease(..),
    DiscogsSearchResults(searchResults),
    DiscogsSearchResult(resultTitle, resultYear, resultType, resultId)
  )

--------------------------------------------------------------------------------

stringToBS :: String -> BS.ByteString
stringToBS = C8.pack

-- https://github.com/haskell/text/issues/218
intToText :: Integral a => a -> T.Text
intToText = T.toStrict . TB.toLazyText . TB.decimal

--------------------------------------------------------------------------------

-- Discogs requires a user agent string to identify the source of requests.
-- https://www.discogs.com/developers#page:home,header:home-general-information
discogsUserAgent :: Option scheme
discogsUserAgent = Req.header "User-Agent" "borscht-hs/0.1 +https://github.com/benrbray/borscht-hs"

-- specify Discogs API version
discogsAccept :: Option scheme
discogsAccept = Req.header "Accept" "application/vnd.discogs.v2.discogs+json"

-- Discogs personal access token
discogsAuth :: String -> Option scheme
discogsAuth token = Req.header "Authorization" (stringToBS $ "Discogs token="++token)

-- given an auth token
discogsHeader :: String -> Option scheme
discogsHeader authKey
    =  discogsUserAgent
    <> discogsAccept
    <> discogsAuth authKey

--------------------------------------------------------------------------------

decodeValue :: FromJSON a => Value -> Maybe a
decodeValue = AT.parseMaybe parseJSON

decodeEither :: FromJSON a => Value -> Either String a
decodeEither = AT.parseEither parseJSON

printExceptT :: Show a => ExceptT a IO b -> IO ()
printExceptT p = runExceptT p >>= \case
                    Left e  -> print e
                    Right x -> pure ()

-- redirect runtime exceptions to ExceptT
-- https://stackoverflow.com/a/26392842/1444650
intercept
  :: ExceptT String IO a
  -> ExceptT String IO a
intercept = handle handler
  where handler :: HttpException -> ExceptT String IO a
        handler = throwError . show

-- allows ExceptT to make requests via `req`
instance Req.MonadHttp (ExceptT String IO) where
    handleHttpException :: HttpException -> ExceptT String IO a
    handleHttpException e = throwError (show e)

--------------------------------------------------------------------------------

data Ctx = Ctx {
    ctxDiscogsAuth   :: String,     -- discogs API key
    ctxSearchOptions :: SearchOpts, -- command line options
    ctxRateGuard     :: IO ()       -- rate-limiter
}

newtype App a = App {
    -- IO (Reader AppContext (Either String a))
    runApp :: ReaderT Ctx (ExceptT String IO) a
} deriving (Monad, Functor, Applicative, (MonadReader Ctx), MonadIO, MonadError String)

-- allows making requests via `req`
instance Req.MonadHttp App where
    handleHttpException :: HttpException -> App a
    handleHttpException e = throwError (show e)

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
    rateGuard     <- asks ctxRateGuard
    searchOptions <- asks ctxSearchOptions

    -- search for releases
    req1 <- requestDiscogsSearch searchOptions

    -- we are looking for specific "release"s, not general "master" listings
    let releases = filter ((== "release") . resultType) (searchResults req1)
    mapM_ (getRelease . resultId) releases
    liftIO $ mapM_ (\s -> do { rateGuard ; print (resultId s) } ) releases

-- given a release ID
getRelease :: Integer -> App DiscogsRelease
getRelease id = do
    authKey   <- asks ctxDiscogsAuth
    rateGuard <- asks ctxRateGuard

    r <- liftIO rateGuard >> requestDiscogsRelease authKey id
    liftIO $ putStrLn ("release title: " ++ show (releaseTitle r))
    return r

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
handleResponse r = case decodeValue (responseBody r) of
    Nothing -> throwError "failed to decode response body"
    Just r  -> return r

discogsJsonReq :: (FromJSON a) => Req.Url scheme -> Option scheme -> App a
discogsJsonReq url params = do
    authKey   <- asks ctxDiscogsAuth
    rateGuard <- asks ctxRateGuard
    r <- liftIO rateGuard >> req GET url NoReqBody jsonResponse (params <> discogsHeader authKey)
    handleResponse r


-- Discogs API: /database/search
requestDiscogsSearch :: SearchOpts -> App DiscogsSearchResults
requestDiscogsSearch opts = do
    -- safe-by-construction URL
    let url = https "api.discogs.com" /: "database" /: "search"
    -- query parameters
    let params =
            "artist" =: ("Mazouni" :: Text) <>
            "track"  =: ("Ecoute moi camarade" :: Text)
    -- perform request, decode response
    discogsJsonReq url params


-- Discogs API: /database/releases/<release_id>
requestDiscogsRelease :: String -> Integer -> App DiscogsRelease
requestDiscogsRelease authKey releaseId = do
    rateGuard <- asks ctxRateGuard
    -- safe-by-construction URL
    let url = https "api.discogs.com" /: "releases" /: intToText releaseId
    -- perform request with no parameters, decode response
    liftIO rateGuard >> discogsJsonReq url mempty

-- oldRequestDiscogsSearch :: String -> SearchOpts -> ExceptT String IO DiscogsSearchResults
-- oldRequestDiscogsSearch authKey opts = do
--     -- safe-by-construction URL
--     let url = https "api.discogs.com" /: "database" /: "search"
--     -- query parameters
--     let params =
--             "artist" =: ("Mazouni" :: Text) <>
--             "track"  =: ("Ecoute moi camarade" :: Text)
--     -- perform request
--     let q = (req GET url NoReqBody jsonResponse discogsHeader :: ExceptT String IO (JsonResponse Value))
--     -- perform request
--     r <- req GET url NoReqBody jsonResponse $
--             discogsHeader       <>      -- request header
--             discogsAuth authKey <>      -- user access token
--             params                      -- query params
--     -- decode JSON response
--     handleResponse r