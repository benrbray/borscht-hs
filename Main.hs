{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Redundant bracket" -}

module Main where


import System.Directory (listDirectory)

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Network.HTTP.Req
    ( (/:),
      defaultHttpConfig,
      https,
      jsonResponse,
      req,
      responseBody,
      runReq,
      POST(POST),
      ReqBodyJson(ReqBodyJson) )

import Data.Aeson.Encode.Pretty (encodePretty)

import qualified Data.ByteString.Lazy.Char8 as B

import Data.Semigroup ((<>))
import Options.Applicative

------------------------------------------------------------

-- SEARCH --------------------------------------------------

data SearchOpts = SearchOpts {
    searchTitle  :: String,
    searchAuthor :: String
} deriving (Eq, Show)

searchCmd :: Parser Commands
searchCmd = SearchCmd <$> (SearchOpts
    <$> strOption
      ( long "title"
          <> metavar "TITLE"
          <> help "song title"
      )
    <*> strOption
      ( long "author"
          <> help "song author"
          <> metavar "AUTHOR"
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
-- main :: IO ()
-- main = runGreetings =<< execParser opts
--   where
--     opts = info (greetings <**> helper)
--       ( fullDesc
--      <> progDesc "Print a greeting for TARGET"
--      <> header "hello - a test for optparse-applicative" )

main :: IO ()
main = runSample =<< execParser opts
  where
    opts = info (sample <**> helper) idm
    --   ( fullDesc
    --  <> progDesc "Print a greeting for TARGET"
    --  <> header "hello - a test for optparse-applicative" )

runSample :: Commands -> IO ()
runSample (SearchCmd opts) = runSearch opts
runSample (ListCmd opts) = printDir $ listDir opts 
runSample NotImplemented = putStrLn "(feature not yet implemented)"


runSearch :: SearchOpts -> IO ()
-- You can either make your monad an instance of 'MonadHttp', or use
-- 'runReq' in any IO-enabled monad without defining new instances.
runSearch opts = runReq defaultHttpConfig $ do
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
      mempty                          -- query params, headers, explicit port number, etc.
  liftIO $ B.putStrLn (encodePretty (responseBody r :: Value))


--------------------------------------------------------------------------------

printDir :: String -> IO ()
printDir dir = do {
    x <- listDirectory dir ;
    print x ;
}