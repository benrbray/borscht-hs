module Borscht.Commands where

import Options.Applicative
import Data.Text (Text)

-- project imports
import Borscht.Req.Discogs (SearchOpts(..))
import Borscht.Util.Functions ((<$$>))

---- SEARCH ------------------------------------------------

searchCmd :: Parser Commands
searchCmd = SearchCmd <$> (SearchOpts
    <$> (optional . strOption)
      ( long "title"
          <> metavar "TITLE"
          <> help "song title"
      )
    <*> (optional . strOption)
      ( long "title"
          <> metavar "TITLE"
          <> help "song title"
      )
    <*> (optional . strOption)
      ( long "type"
          <> help "song type"
          <> metavar "TYPE"
      )
    )

-- LIST ----------------------------------------------------

data ListOpts = ListOpts {
    listDir :: Text
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
  | TestCmd
  | TestDbCmd
  | NotImplemented
  deriving (Eq, Show)