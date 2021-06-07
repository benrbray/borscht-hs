{- HLINT ignore "Redundant bracket" -}

module Main where

-- control
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Control.Monad.Trans.Class (lift)
import Control.Monad (guard, liftM, forever, mzero, msum, when, join, ap)

-- monad transformers
import Control.Monad.Trans.Except (ExceptT(ExceptT))
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Reader (MonadReader)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Except (MonadError)

-- algebraic
import Data.Semigroup ((<>))

-- optparse-applicative
import Options.Applicative
    ( (<**>),
      command,
      commandGroup,
      hidden,
      idm,
      info,
      progDesc,
      subparser,
      execParser,
      helper,
      Alternative((<|>)),
      Parser )

-- project imports
import Borscht.Commands
import Borscht.Commands.SearchCmd (runSearchCmd)
import Borscht.Commands.ListCmd (runListDir)
import Borscht.Commands.TestCmd (runTestCmd)

-- rate limit
import Data.Time.Units ( Second )
import Borscht.Util.RateLimit (rateLimitInvocation)

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
      -- test commands
      <|> subparser
       ( commandGroup "Test Commands:"
      <> command "test"
         (info (pure TestCmd)
               (progDesc "Endpoint for testing small functions."))
      <> hidden
       )

--------------------------------------------------------------------------------


main :: IO ()
main = chooseCommand =<< execParser opts
  where
    opts = info (sample <**> helper) idm
    --   ( fullDesc
    --  <> progDesc "Print a greeting for TARGET"
    --  <> header "hello - a test for optparse-applicative" )

chooseCommand :: Commands -> IO ()
chooseCommand (SearchCmd opts) = runSearchCmd opts
chooseCommand (ListCmd opts) = runListDir opts
chooseCommand TestCmd = runTestCmd
chooseCommand _ = putStrLn "(feature not yet implemented)"

--------------------------------------------------------------------------------

ensure :: Alternative f => (a -> Bool) -> a -> f a
ensure p a = a <$ guard (p a)

--------------------------------------------

-- main parses and validates the command line arguments,
-- and sends them off to the program entry point
-- main :: IO (Maybe ())
-- main = runMaybeT $
--            do liftIO $ putStr "hello"
--               p <- askPassphrase
--               when (p == "SECRET") mzero

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