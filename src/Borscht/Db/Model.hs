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

module Borscht.Db.Model where

-- database
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
-- text
import Data.Text (Text)
-- misc
import Data.Kind (Type)
-- monad
import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Logger (NoLoggingT)
import UnliftIO (MonadUnliftIO)
import Conduit (ResourceT)

--------------------------------------------------------------------------------

-- TODO (2021-06-23) use persistent-mtl instead?
type SqliteAction m a = ReaderT SqlBackend (NoLoggingT (ResourceT m)) a

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name Text
    age Int Maybe
    deriving Show
BlogPost
    title Text
    authorId PersonId
    deriving Show
Artist
    name Text
    UniqueArtistName name
    deriving Show
Track
    title Text
    artist ArtistId
    deriving Show
|]

migrate
    :: Text  -- database file path or ":memory:"
    -> IO () -- database action
migrate db = runSqlite db $
    runMigration migrateAll
