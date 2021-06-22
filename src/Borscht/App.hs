{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Borscht.App where

--------------------------------------------------------------------------------

-- requests
import qualified Network.HTTP.Req as Req

-- monad transformers
import Control.Monad (Monad)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.Except (MonadError, ExceptT, throwError)

-- borscht
import Borscht.Commands (SearchOpts)

--------------------------------------------------------------------------------

data Ctx = Ctx {
    ctxDiscogsAuth   :: String,     -- discogs API key
    ctxSearchOptions :: SearchOpts, -- command line options
    ctxRateGuard     :: IO ()       -- rate-limiter
}

newtype App a = App {
    -- IO (Reader AppContext (Either String a))
    runApp :: ReaderT Ctx (ExceptT String IO) a
} deriving (Monad, Functor, Applicative, (MonadReader Ctx), MonadIO, MonadError String)

-- allows making requests via `req`
instance Req.MonadHttp App where
    handleHttpException :: Req.HttpException -> App a
    handleHttpException e = throwError (show e)