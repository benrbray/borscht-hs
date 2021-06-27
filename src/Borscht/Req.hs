module Borscht.Req where

--------------------------------------------------------------------------------

-- aeson
import Data.Aeson
import qualified Data.Aeson.Types as AT

-- monads
import Control.Exception.Lifted (handle)
import Control.Monad.Except (ExceptT(..), runExceptT, MonadError(..))
import Control.Monad.Trans (MonadIO, liftIO)

-- pretty-simple
import Text.Pretty.Simple (pPrint)

-- req
import qualified Network.HTTP.Req as Req (HttpException, JsonResponse, responseBody)

--------------------------------------------------------------------------------

printExceptT :: Show a => ExceptT a IO b -> IO ()
printExceptT p = runExceptT p >>= \case
                    Left e  -> print e
                    Right _ -> pure ()

-- redirect runtime exceptions to ExceptT
-- https://stackoverflow.com/a/26392842/1444650
intercept
  :: ExceptT String IO a
  -> ExceptT String IO a
intercept = handle handler
  where handler :: Req.HttpException -> ExceptT String IO a
        handler = throwError . show

--------------------------------------------------------------------------------

decodeValue :: FromJSON a => Value -> Maybe a
decodeValue = AT.parseMaybe parseJSON

decodeEither :: FromJSON a => Value -> Either String a
decodeEither = AT.parseEither parseJSON

handleResponse
    :: (MonadIO m, MonadError String m, FromJSON a)
    => Req.JsonResponse Value -> m a
handleResponse resp = case decodeEither (Req.responseBody resp) of
    Left e  -> do liftIO $ pPrint resp
                  throwError $ "[failed to decode response body] " ++ e
    Right r -> return r