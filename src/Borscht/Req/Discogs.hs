{- HLINT ignore "Redundant bracket" -}
{-# LANGUAGE RecordWildCards #-}

module Borscht.Req.Discogs (
    requestDiscogsSearch,
    requestDiscogsRelease,
    requestDiscogsMaster,
    SearchOpts(..), matchArtist, matchTitle, matchYear, matchType
) where

--------------------------------------------------------------------------------

-- data
import Data.Default (Default, def)
import Control.Applicative ( Alternative((<|>)) )

-- text
import Data.Text (Text)
import qualified Data.ByteString.Char8 as C8 (pack)

-- aeson
import Data.Aeson ( FromJSON )

-- requests
import qualified Network.HTTP.Req as Req
import Network.HTTP.Req(
      (/:), (=:),
      https, GET(GET), req, jsonResponse,
      NoReqBody(NoReqBody),
      Option
    )

-- monad transformers
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)

-- borscht
import Borscht.App
    (App(..), Ctx(..))
import Borscht.Req
    (handleResponse)
import Borscht.Req.Discogs.JSON ( 
    DiscogsSearchResults, 
    DiscogsRelease, DiscogsMasterRelease
  )
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

data SearchOpts = SearchOpts {
        searchArtist :: Maybe Text,
        searchTitle  :: Maybe Text,
        searchYear   :: Maybe Text,
        searchType   :: Maybe Text -- TODO: use a datatype here (or DataKinds string literals)
    } deriving (Eq, Ord, Read, Show)

instance Default SearchOpts where
    def = SearchOpts Nothing Nothing Nothing Nothing

instance Semigroup SearchOpts where
    SearchOpts a1 t1 y1 typ1 <> SearchOpts a2 t2 y2 typ2
        = SearchOpts (a1 <|> a2) (t1 <|> t2) (y1 <|> y2) (typ1 <|> typ2)

instance Monoid SearchOpts where mempty = def

matchArtist :: Text -> SearchOpts
matchArtist a = def { searchArtist = Just a }

matchTitle :: Text -> SearchOpts
matchTitle t = def { searchTitle = Just t }

matchYear :: Text -> SearchOpts
matchYear y = def { searchYear = Just y }

matchType :: Text -> SearchOpts
matchType y = def { searchType = Just y }

-- Discogs API: /database/search
-- returns releases only
requestDiscogsSearch :: SearchOpts -> App DiscogsSearchResults
requestDiscogsSearch query = do
    -- safe-by-construction URL
    let url = https "api.discogs.com" /: "database" /: "search"

    let args = [
            ("artist" , searchArtist query),
            ("track"  , searchTitle query),
            ("year"   , searchYear query),
            ("type"   , searchType query)
          ]
    
    -- like catMaybe on the second argument
    -- TODO (2021-06-27) use lenses here?
    let justArgs = [ (key,v) | arg@(key, Just v) <- args ]

    liftIO $ do
        putStr "Just Args:\t" 
        print justArgs

    let params   = (map (uncurry (=:)) justArgs)
    let option   = (foldl (<>) mempty params)
    
    -- perform request, decode response
    discogsJsonReq url option

-- Discogs API: /database/releases/<release_id>
requestDiscogsRelease :: Integer -> App DiscogsRelease
requestDiscogsRelease rid = do
    -- safe-by-construction URL
    let url = https "api.discogs.com" /: "releases" /: intToText rid
    -- perform request with no parameters, decode response
    discogsJsonReq url mempty

-- Discogs API: /database/masters/<master_id>
requestDiscogsMaster :: Integer -> App DiscogsMasterRelease
requestDiscogsMaster rid = do
    -- safe-by-construction URL
    let url = https "api.discogs.com" /: "masters" /: intToText rid
    -- perform request with no parameters, decode response
    discogsJsonReq url mempty