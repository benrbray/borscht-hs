{- HLINT ignore "Redundant bracket" -}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Borscht.Req.Discogs.JSON where

--------------------------------------------------------------------------------

import GHC.Generics ( Generic )
import Data.Text (Text)
import Data.Sequence as Seq ( fromList )

import Data.Aeson
import qualified Data.Aeson.Types as AT

import Text.RawString.QQ
import qualified Data.ByteString.Lazy.Char8 as BS

--------------------------------------------------------------------------------

data DiscogsRelease = DiscogsRelease {
    releaseId     :: Integer,
    releaseTitle  :: Text,
    releaseTracks :: [DiscogsTrack]
} deriving (Generic, Show)

instance ToJSON DiscogsRelease where
    -- No need to provide a toJSON implementation.  For efficiency, we write a
    -- simple toEncoding implementation, as the default version uses toJSON.
    --toEncoding = genericToEncoding defaultOptions
    toEncoding DiscogsRelease{..} = pairs $
        "id"        .= releaseId    <>
        "title"     .= releaseTitle <>
        "tracklist" .= releaseTracks

instance FromJSON DiscogsRelease where
    parseJSON = withObject "DiscogsRelease" $ \v -> do
        releaseId     <- v .: "id"
        releaseTitle  <- v .: "title"
        releaseTracks <- v .: "tracklist"
        return DiscogsRelease{..}

--------------------------------------------------------------------------------

data DiscogsTrack = DiscogsTrack {
    trackPosition :: Text,
    trackType     :: Text,
    trackTitle    :: Text,
    trackDuration :: Text
} deriving (Generic, Show)

instance ToJSON DiscogsTrack where
    -- No need to provide a toJSON implementation.  For efficiency, we write a
    -- simple toEncoding implementation, as the default version uses toJSON.
    --toEncoding = genericToEncoding defaultOptions
    toEncoding DiscogsTrack{..} = pairs $
        "position" .= trackPosition  <>
        "type"     .= trackType      <>
        "title"    .= trackTitle     <>
        "duration" .= trackDuration

instance FromJSON DiscogsTrack where
    parseJSON = withObject "DiscogsTrack" $ \v -> do
        trackPosition <- v .: "position"
        trackType     <- v .: "type_"
        trackTitle    <- v .: "title"
        trackDuration <- v .: "duration"
        return DiscogsTrack{..}

-- DiscogsSearchResult ---------------------------------------------------------

data DiscogsSearchResult = DiscogsSearchResult {
    resultId          :: Integer,
    resultGenre       :: [Text],
    resultStyle       :: [Text],
    resultTitle       :: Text,
    resultYear        :: Text,
    resultLabel       :: [Text],
    resultResourceUrl :: Text,
    resultCountry     :: Text,
    resultCoverImage  :: Text,
    resultType        :: Text
} deriving (Generic, Show)

instance ToJSON DiscogsSearchResult where
    -- No need to provide a toJSON implementation.  For efficiency, we write a
    -- simple toEncoding implementation, as the default version uses toJSON.
    --toEncoding = genericToEncoding defaultOptions
    toEncoding DiscogsSearchResult{..} = pairs $
        "id"           .= resultId            <>
        "genre"        .= resultGenre         <>
        "style"        .= resultStyle         <>
        "title"        .= resultTitle         <>
        "year"         .= resultYear          <>
        "label"        .= resultLabel         <>
        "resource_url" .= resultResourceUrl   <>
        "country"      .= resultCountry       <>
        "cover_image"  .= resultCoverImage    <>
        "type"         .= resultType

instance FromJSON DiscogsSearchResult where
    parseJSON = withObject "DiscogsSearchResult" $ \v -> do
        resultId          <- v .: "id"
        resultGenre       <- v .: "genre"
        resultStyle       <- v .: "style"
        resultTitle       <- v .: "title"
        resultYear        <- v .: "year"
        resultLabel       <- v .: "label"
        resultResourceUrl <- v .: "resource_url"
        resultCountry     <- v .: "country"
        resultCoverImage  <- v .: "cover_image"
        resultType        <- v .: "type"
        return DiscogsSearchResult{..}

-- DiscogsPagination -----------------------------------------------------------

data DiscogsPagination = DiscogsPagination {
    items :: Int,
    page :: Int,
    per_page :: Int,
    pages :: Int
} deriving (Generic, Show)

instance ToJSON DiscogsPagination where
    -- No need to provide a toJSON implementation.  For efficiency, we write a
    -- simple toEncoding implementation, as the default version uses toJSON.
    toEncoding = genericToEncoding defaultOptions

instance FromJSON DiscogsPagination where
    parseJSON = withObject "DiscogsPagination" $ \v -> DiscogsPagination
        <$> v .: "items"
        <*> v .: "page"
        <*> v .: "per_page"
        <*> v .: "pages"

-- DiscogsSearchResults --------------------------------------------------------

data DiscogsSearchResults = DiscogsSearchResults {
    searchPagination :: Maybe DiscogsPagination,
    searchResults :: [DiscogsSearchResult]
} deriving (Generic, Show)

instance FromJSON DiscogsSearchResults where
    parseJSON = withObject "DiscogsSearchResults" $ \v -> DiscogsSearchResults
        <$> v .:? "pagination"
        <*> v .:  "results"

instance ToJSON DiscogsSearchResults where
    -- No need to provide a toJSON implementation.  For efficiency, we write a
    -- simple toEncoding implementation, as the default version uses toJSON.
    toEncoding = genericToEncoding defaultOptions

--------------------------------------------------------------------------------