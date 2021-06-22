{- HLINT ignore "Redundant bracket" -}
{-# LANGUAGE RecordWildCards #-}

module Borscht.Req.Discogs (
    requestDiscogsSearch,
    requestDiscogsRelease
) where

--------------------------------------------------------------------------------

-- text
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C8 (pack)

-- aeson
import Data.Aeson

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

-- borscht
import Borscht.App
    (App(..), Ctx(..))
import Borscht.Req
    (handleResponse)
import Borscht.Req.Discogs.JSON
    ( DiscogsSearchResults, DiscogsRelease )
import Borscht.Commands
    (SearchOpts, searchTitle, searchArtist)
import Borscht.Util.Functions
    (intToText)

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
discogsAuth token = Req.header "Authorization" (C8.pack $ "Discogs token="++token)

-- given an auth token
discogsHeader :: String -> Option scheme
discogsHeader authKey
    =  discogsUserAgent
    <> discogsAccept
    <> discogsAuth authKey

--------------------------------------------------------------------------------

-- Performs a GET request with the appropriate headers for the specified
-- discogs URL and parameters, expecting a JSON object in response.
discogsJsonReq :: (FromJSON a) => Req.Url scheme -> Option scheme -> App a
discogsJsonReq url params = do
    authKey  <- asks ctxDiscogsAuth
    liftIO  =<< asks ctxRateGuard 
    handleResponse
        =<< req GET url NoReqBody jsonResponse (params <> discogsHeader authKey)

--------------------------------------------------------------------------------

-- Discogs API: /database/search
requestDiscogsSearch :: SearchOpts -> App DiscogsSearchResults
requestDiscogsSearch opts = do
    -- safe-by-construction URL
    let url = https "api.discogs.com" /: "database" /: "search"
    -- query parameters
    query <- asks ctxSearchOptions
    liftIO $ print ("artist: " ++ T.unpack (searchArtist query)) 
    liftIO $ print ("track:  " ++ T.unpack (searchTitle query)) 
    let params =
            "artist" =: searchArtist query <>
            "track"  =: searchTitle query
    -- perform request, decode response
    discogsJsonReq url params

-- Discogs API: /database/releases/<release_id>
requestDiscogsRelease :: Integer -> App DiscogsRelease
requestDiscogsRelease rid = do
    -- safe-by-construction URL
    let url = https "api.discogs.com" /: "releases" /: intToText rid
    -- perform request with no parameters, decode response
    discogsJsonReq url mempty