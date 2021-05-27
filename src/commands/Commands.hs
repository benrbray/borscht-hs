module Commands where

import Options.Applicative

---- SEARCH ------------------------------------------------

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
  | TestCmd
  | NotImplemented
  deriving (Eq, Show)