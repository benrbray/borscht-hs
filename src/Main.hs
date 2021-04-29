{- HLINT ignore "Redundant bracket" -}

module Main where

-- system
import System.Directory (listDirectory)
import System.Environment (lookupEnv)

-- control
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Control.Monad.Trans.Class (lift)
import Control.Monad (guard, liftM, forever, mzero, msum, when)

-- aeson
import Data.Aeson
import qualified Data.Aeson.Types as AT
import Data.Aeson.Encode.Pretty (encodePretty)

-- requests
import Network.HTTP.Req as Req
import Network.HTTP.Req
    ( (/:),
      defaultHttpConfig,
      https,
      jsonResponse,
      req,
      responseBody,
      runReq,
      POST(POST), GET(GET),
      ReqBodyJson(ReqBodyJson),
      header )

-- ugh, strings
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (chr)
import Data.ByteString as BS  (ByteString)
import Data.ByteString.Char8 as C8 (pack)
import qualified Data.ByteString.Lazy.Char8 as B

-- algebraic
import Data.Semigroup ((<>))

-- optparse-applicative
import Options.Applicative

-- project imports
import Discogs (DiscogsSearchResults)

------------------------------------------------------------

-- SEARCH --------------------------------------------------

data SearchOpts = SearchOpts {
    searchTitle  :: String,
    searchArtist :: String
} deriving (Eq, Show)

searchCmd :: Parser Commands
searchCmd = SearchCmd <$> (SearchOpts
    <$> strOption
      ( long "title"
          <> metavar "TITLE"
          <> help "song title"
      )
    <*> strOption
      ( long "artist"
          <> help "song artist"
          <> metavar "ARTIST"
      ) )

-- LIST ----------------------------------------------------

data ListOpts = ListOpts {
    listDir :: String
} deriving (Eq, Show)

listCmd :: Parser Commands
listCmd = ListCmd <$> (ListOpts
    <$> strOption
      ( long "dir"
          <> metavar "DIRECTORY"
          <> help "directory to search"
      ) )

------------------------------------------------------------

data Commands
  = SearchCmd SearchOpts
  | ListCmd ListOpts
  | NotImplemented
  deriving (Eq, Show)

------------------------------------------------------------

sample :: Parser Commands
sample = subparser
      -- library commands
       ( command "search"
         (info searchCmd
               (progDesc "Search the Discogs/MusicBrainz databases."))
      <> command "list"
         (info listCmd
               (progDesc "List songs in your database."))
      <> command "import"
         (info (pure NotImplemented)
               (progDesc "Add songs to your library."))
      <> command "remove"
         (info (pure NotImplemented)
               (progDesc "Remove songs from your library."))
      <> commandGroup "Library Commands:"
      <> hidden
       )
      -- playback commands
      <|> subparser
       ( commandGroup "Playback Commands:"
      <> command "play"
         (info (pure NotImplemented)
               (progDesc "Play your music database."))
      <> command "shuffle"
         (info (pure NotImplemented)
               (progDesc "Shuffle your music."))
      <> hidden
       )

--------------------------------------------------------------------------------

-- main parses and validates the command line arguments,
-- and sends them off to the program entry point
-- main :: IO (Maybe ())
-- main = runMaybeT $
--            do liftIO $ putStr "hello"
--               p <- askPassphrase
--               when (p == "SECRET") mzero

main :: IO ()
main = chooseCommand =<< execParser opts
  where
    opts = info (sample <**> helper) idm
    --   ( fullDesc
    --  <> progDesc "Print a greeting for TARGET"
    --  <> header "hello - a test for optparse-applicative" )

chooseCommand :: Commands -> IO ()
chooseCommand (SearchCmd opts) = runSearch opts
chooseCommand (ListCmd opts) = printDir $ listDir opts
chooseCommand NotImplemented = putStrLn "(feature not yet implemented)"

--------------------------------------------------------------------------------

stringToBS :: String -> ByteString
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


dummySearch :: SearchOpts -> IO ()
-- You can either make your monad an instance of 'MonadHttp', or use
-- 'runReq' in any IO-enabled monad without defining new instances.
dummySearch opts = runReq defaultHttpConfig $ do
  let payload =
        object
          [ "foo" .= (10 :: Int),
            "bar" .= (20 :: Int)
          ]
  -- One function—full power and flexibility, automatic retrying on timeouts
  -- and such, automatic connection sharing.
  r <-
    req
      POST                            -- method
      (https "httpbin.org" /: "post") -- safe by construction URL
      (ReqBodyJson payload)           -- use built-in options or add your own
      jsonResponse                    -- specify how to interpret response
      discogsUserAgent                -- query params, headers, explicit port number, etc.
  liftIO $ B.putStrLn (encodePretty (responseBody r :: Value))


--------------------------------------------------------------------------------

printDir :: String -> IO ()
printDir dir = do {
    x <- listDirectory dir ;
    print x ;
}

--------------------------------------------------------------------------------

ensure :: Alternative f => (a -> Bool) -> a -> f a
ensure p a = a <$ guard (p a)

--------------------------------------------

getPassphrase :: MaybeT IO String
getPassphrase = do
    s <- lift getLine
    ensure isValid s
    return s

-- The validation test could be anything we want it to be.
isValid :: String -> Bool
isValid s = length s >= 4

askPassphrase :: MaybeT IO String
askPassphrase = do 
                 lift $ putStrLn "Insert your new passphrase:"
                 value <- msum $ repeat getPassphrase
                 lift $ putStrLn "Storing in database..."
                 return value