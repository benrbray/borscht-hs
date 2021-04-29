{- HLINT ignore "Redundant bracket" -}

module Main where

-- control
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Control.Monad.Trans.Class (lift)
import Control.Monad (guard, liftM, forever, mzero, msum, when)

-- -- aeson
-- import Data.Aeson
-- import qualified Data.Aeson.Types as AT
-- import Data.Aeson.Encode.Pretty (encodePretty)

-- -- requests
-- import Network.HTTP.Req as Req
-- import Network.HTTP.Req
--     ( (/:),
--       defaultHttpConfig,
--       https,
--       jsonResponse,
--       req,
--       responseBody,
--       runReq,
--       POST(POST), GET(GET),
--       ReqBodyJson(ReqBodyJson),
--       header )

-- ugh, strings
-- import Data.Text (Text)
-- import qualified Data.Text as T
-- import Data.Char (chr)
-- import Data.ByteString as BS  (ByteString)
-- import Data.ByteString.Char8 as C8 (pack)
-- import qualified Data.ByteString.Lazy.Char8 as B

-- algebraic
import Data.Semigroup ((<>))

-- optparse-applicative
import Options.Applicative

-- project imports
import Commands
import SearchCmd (runSearch)
import ListCmd (runListDir)

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
chooseCommand (ListCmd opts) = runListDir opts
chooseCommand NotImplemented = putStrLn "(feature not yet implemented)"

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