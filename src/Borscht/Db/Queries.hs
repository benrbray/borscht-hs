{- Required by Persistent, https://www.yesodweb.com/book/persistent -}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}

module Borscht.Db.Queries where

--------------------------------------------------------------------------------

import Data.Kind (Type)

-- text
import Data.Text (Text)
import qualified Data.Text as T

-- monad
import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Logger (NoLoggingT)
import UnliftIO (MonadUnliftIO)
import Conduit (ResourceT)

-- persistent
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

-- borscht
import qualified Borscht.Db.Model as Db

--------------------------------------------------------------------------------

queryArtistName :: (MonadUnliftIO m)
    => Text -> Db.SqliteAction m (Key Db.Artist)
queryArtistName name = do
    maybeArtist <- getBy $ Db.UniqueArtistName name
    case maybeArtist of 
        Nothing -> insert $ Db.Artist name
        Just (Entity artistId artist) -> pure artistId

queryTrackTitle :: (MonadUnliftIO m)
    => Text -> Db.SqliteAction m (Key Db.Artist)
queryTrackTitle title = do
    maybeArtist <- getBy $ Db.UniqueArtistName title
    case maybeArtist of 
        Nothing -> insert $ Db.Artist title
        Just (Entity artistId artist) -> pure artistId