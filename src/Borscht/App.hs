{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Borscht.App where

--------------------------------------------------------------------------------

-- system
import System.Environment (lookupEnv)

-- requests
import qualified Network.HTTP.Req as Req

-- rate limit
import Data.Time.Units ( Millisecond )
import Borscht.Util.RateLimit (rateLimitInvocation)

-- monad transformers
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.Except (MonadError, ExceptT, throwError)

--------------------------------------------------------------------------------

data Ctx = Ctx {
    ctxDiscogsAuth   :: String,     -- discogs API key
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

--------------------------------------------------------------------------------

throwMaybe :: (MonadError b m) => b -> Maybe a -> m a
throwMaybe e Nothing  = throwError e
throwMaybe _ (Just x) = pure x

getEnv :: (MonadIO m, MonadError String m) => String -> m String
getEnv name
    = liftIO (lookupEnv name)
    >>= throwMaybe ("environment variable" ++ name ++ " not found")

prepareCtx :: (MonadIO m, MonadError String m) => m Ctx
prepareCtx = do
    -- prepare api key
    authKey <- getEnv "DISCOGS_KEY"
    -- prepare rate limiter
    rateGuard <- liftIO $ rateLimitInvocation (1200 :: Millisecond) (pure :: () -> IO ())
    -- prepare request context
    return Ctx {
        ctxDiscogsAuth   = authKey,
        ctxRateGuard     = rateGuard ()
    }