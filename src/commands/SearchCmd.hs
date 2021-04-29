{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use <$>" -}

module SearchCmd where

-- system
import System.Environment (lookupEnv)

-- aeson
import Data.Aeson
import qualified Data.Aeson.Types as AT
import Data.Aeson.Encode.Pretty (encodePretty)

-- control
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Except (ExceptT(ExceptT), throwE, catchE, runExceptT)

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

import Data.Text (Text, intercalate)
import Text.Printf
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Char (chr)
import qualified Data.ByteString as BS  (ByteString)
import qualified Data.ByteString.Char8 as C8 (pack)
import qualified Data.ByteString.Lazy.Char8 as B

-- project imports
import Discogs (
    DiscogsSearchResults(results),
    DiscogsSearchResult(resultTitle, resultYear)
  )
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
        Just authKey -> runSearch2 authKey opts --void $ runExceptT $ do
        --     result <- lift $ makeDiscogsSearchRequest authKey opts
        --     return _

        --runExceptT $ do
            -- catchE $ doStuff catchString
            -- --catchE x catchString --displaySearchResults =<< makeDiscogsSearchRequest authKey opts
            -- return ()

runSearch2 :: String -> SearchOpts -> IO ()
runSearch2 authKey opts = void $ runExceptT $ do
    x <- lift $ makeDiscogsSearchRequest authKey opts
    lift $ case x of
        Nothing -> putStrLn "empty result"
        Just  r -> displaySearchResults r

displaySearchResults :: DiscogsSearchResults -> IO ()
displaySearchResults x = mapM_ displaySearchResult (results x)

displaySearchResult :: DiscogsSearchResult -> IO ()
displaySearchResult x = do
    T.putStrLn $ intercalate (T.pack " -- ") [resultTitle x, resultYear x]

-- You can either make your monad an instance of 'MonadHttp', or use
-- 'runReq' in any IO-enabled monad without defining new instances.
makeDiscogsSearchRequest :: String -> SearchOpts -> IO (Maybe DiscogsSearchResults)
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
    return (decodeValue (responseBody r :: Value) :: Maybe DiscogsSearchResults)


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

validString :: String -> Either String String
validString s = if head s == 'a' then Right s else Left "string doesn't start with 'a'"

-- runTest :: IO (Except String ())
runTest :: ExceptT String IO ()
runTest = do
    x <- lift getLine

    ExceptT $ pure $ validString x

    lift $ do
        putStrLn "running test..."
        putStrLn x