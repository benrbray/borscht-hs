{- HLINT ignore "Redundant bracket" -}
{-# LANGUAGE RecordWildCards #-}

module Borscht.Req.MusicBrainz (
    searchRecording
) where

--------------------------------------------------------------------------------

-- text
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C8 (pack)

-- aeson
import Data.Aeson

-- requests
import qualified Network.HTTP.Req as Req
import Network.HTTP.Req (
    (/:), (=:),
    req, GET(GET),
    NoReqBody(NoReqBody), jsonResponse, responseBody
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
    ( DiscogsRelease )
import Borscht.Req.Discogs
    ( SearchOpts(..) )

--------------------------------------------------------------------------------

-- MusicBrainz requires a user agent string to identify the source of requests.
-- https://musicbrainz.org/doc/MusicBrainz_API/Rate_Limiting#Provide_meaningful_User-Agent_strings
mbzUserAgent :: Req.Option scheme
mbzUserAgent = Req.header "User-Agent" "borscht-hs/0.1 ( https://github.com/benrbray/borscht-hs )"

-- standard header for making MusicBrainz requests
mbzHeader :: Req.Option scheme
mbzHeader = mbzUserAgent

--------------------------------------------------------------------------------

-- Performs a GET request with the appropriate headers for the specified
-- discogs URL and parameters, expecting a JSON object in response.
jsonReq :: (FromJSON a) => Req.Url scheme -> Req.Option scheme -> App a
jsonReq url params = do
    liftIO  =<< asks ctxRateGuard 
    r <- req GET url NoReqBody jsonResponse (params <> mbzHeader)
    liftIO $ print r
    handleResponse r

--------------------------------------------------------------------------------

-- | Search for a release given partial information about a track.
-- MusicBrainz API: /database/release-group/<release_id>
searchRecording :: SearchOpts -> App DiscogsRelease
searchRecording query = do
    -- safe-by-construction URL
    let url = Req.https "api.discogs.com" /: "database" /: "search"
    -- query parameters
    liftIO $ print query
    -- TODO MusicBrainz supports Lucene search queries,
    -- so we must escape Lucene reserved characters 
    -- https://lucene.apache.org/core/4_3_0/queryparser/org/apache/lucene/queryparser/classic/package-summary.html#Escaping_Special_Characters
    let params =
            "artistname" =: searchArtist query <>
            "recording"  =: searchTitle query
    -- perform request, decode response
    jsonReq url params