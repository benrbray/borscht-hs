{- From Persistent, https://www.yesodweb.com/book/persistent -}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Borscht.Commands.TestDbCmd (
    runTestDbCmd
) where

import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import Data.Kind (Type)
import Data.Text (Text)
import UnliftIO (MonadUnliftIO)

import Control.Monad.Reader (ReaderT)
import Control.Monad.Logger (NoLoggingT)
import Conduit (ResourceT)

--------------------------------------------------------------------------------

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
Album
    title Text
    albumArtist ArtistId
    deriving Show
Track
    title Text
    artist ArtistId
    deriving Show
|]

queryArtistId :: forall (m :: Type -> Type). (MonadUnliftIO m)
    => ReaderT SqlBackend (NoLoggingT (ResourceT m)) (Key Artist)
queryArtistId = do
    maybeArtist <- getBy $ UniqueArtistName "Mazouni"
    case maybeArtist of 
        Nothing -> insert $ Artist "Mazouni"
        Just (Entity artistId artist) -> pure artistId

runTestDbCmd :: IO ()
runTestDbCmd = runSqlite "music.db" $ do
    runMigration migrateAll

    johnId <- insert $ Person "John Doe" $ Just 35
    janeId <- insert $ Person "Jane Doe" Nothing

    -- get artist by name, if exists
    artistId <- queryArtistId
    trackId <- insert $ Track "Ecoute Moi Camarade" artistId 

    insert $ BlogPost "My fr1st p0st" johnId
    insert $ BlogPost "One more for good measure" johnId

    favoriteSong <- selectList [TrackArtist ==. artistId] [LimitTo 1]
    liftIO $ print (favoriteSong :: [Entity Track])

    oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
    liftIO $ print (oneJohnPost :: [Entity BlogPost])

    john <- get johnId
    liftIO $ print (john :: Maybe Person)

    delete janeId
    deleteWhere [BlogPostAuthorId ==. johnId]