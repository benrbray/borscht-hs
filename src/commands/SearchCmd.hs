{- HLINT ignore "Redundant bracket" -}

module SearchCmd where

-- system
import System.Environment (lookupEnv)

-- aeson
import Data.Aeson
import qualified Data.Aeson.Types as AT
import Data.Aeson.Encode.Pretty (encodePretty)

-- control
import Control.Monad.IO.Class (liftIO)

-- requests
import qualified Network.HTTP.Req as Req
import Network.HTTP.Req
    ( (/:),
      (=:),
      defaultHttpConfig,
      https,
      jsonResponse,
      req,
      responseBody,
      runReq,
      GET(GET),
      NoReqBody(NoReqBody),
      Option )

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (chr)
import qualified Data.ByteString as BS  (ByteString)
import qualified Data.ByteString.Char8 as C8 (pack)
import qualified Data.ByteString.Lazy.Char8 as B

-- project imports
import Discogs (DiscogsSearchResults)
import Commands (SearchOpts)

--------------------------------------------------------------------------------

stringToBS :: String -> BS.ByteString
stringToBS = C8.pack

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

discogsHeader :: Option scheme
discogsHeader = discogsUserAgent <>
                discogsAccept

--------------------------------------------------------------------------------

decodeValue :: FromJSON a => Value -> Maybe a
decodeValue = AT.parseMaybe parseJSON

decodeEither :: FromJSON a => Value -> Either String a
decodeEither = AT.parseEither parseJSON

runSearch :: SearchOpts -> IO ()
runSearch opts = do
    print "looking up auth key..."
    lookupEnv "DISCOGS_KEY" >>= \case
        Nothing      -> B.putStrLn $ "key not found."
        Just authKey -> makeDiscogsSearchRequest authKey opts

-- You can either make your monad an instance of 'MonadHttp', or use
-- 'runReq' in any IO-enabled monad without defining new instances.
makeDiscogsSearchRequest :: String -> SearchOpts -> IO ()
makeDiscogsSearchRequest authKey opts = runReq defaultHttpConfig $ do
  -- safe-by-construction URL
  let url = https "api.discogs.com" /: "database" /: "search"
  -- query parameters
  let params =
        "artist" =: ("Mazouni" :: Text) <>
        "track"  =: ("Ecoute moi camarade" :: Text)
  -- One function—full power and flexibility, automatic retrying on timeouts
  -- and such, automatic connection sharing.
  r <- req GET url NoReqBody jsonResponse $
          discogsHeader       <>      -- request header
          discogsAuth authKey <>      -- user access token
          params                      -- query params
  liftIO $ do
    B.putStrLn (encodePretty (responseBody r :: Value))
    print (decodeValue (responseBody r :: Value) :: Maybe DiscogsSearchResults)


-- dummySearch :: SearchOpts -> IO ()
-- -- You can either make your monad an instance of 'MonadHttp', or use
-- -- 'runReq' in any IO-enabled monad without defining new instances.
-- dummySearch opts = runReq defaultHttpConfig $ do
--   let payload =
--         object
--           [ "foo" .= (10 :: Int),
--             "bar" .= (20 :: Int)
--           ]
--   -- One function—full power and flexibility, automatic retrying on timeouts
--   -- and such, automatic connection sharing.
--   r <-
--     req
--       POST                            -- method
--       (https "httpbin.org" /: "post") -- safe by construction URL
--       (ReqBodyJson payload)           -- use built-in options or add your own
--       jsonResponse                    -- specify how to interpret response
--       discogsUserAgent                -- query params, headers, explicit port number, etc.
--   liftIO $ B.putStrLn (encodePretty (responseBody r :: Value))