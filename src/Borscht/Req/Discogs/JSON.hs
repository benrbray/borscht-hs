{- HLINT ignore "Redundant bracket" -}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Borscht.Req.Discogs.JSON where

--------------------------------------------------------------------------------

import GHC.Generics ( Generic )

import Data.Text (Text)
import qualified Data.Text as T

import Data.Aeson
import Data.Aeson.Types as AT

--------------------------------------------------------------------------------

-- data type for elements of the "artists" array of a release 
data DiscogsReleaseArtist = DiscogsReleaseArtist {
    releaseArtistId   :: Integer,
    releaseArtistName :: Text,
    releaseArtistRole :: Text
} deriving (Generic, Show)

instance ToJSON DiscogsReleaseArtist where
    toEncoding DiscogsReleaseArtist{..} = pairs $
        "id"   .= releaseArtistId    <>
        "name" .= releaseArtistName <>
        "role" .= releaseArtistRole

instance FromJSON DiscogsReleaseArtist where
    parseJSON = withObject "DiscogsReleaseArtist" $ \v -> do
        releaseArtistId   <- v .: "id"
        releaseArtistName <- v .: "name"
        releaseArtistRole <- v .: "role"
        return DiscogsReleaseArtist{..}

--------------------------------------------------------------------------------

data DiscogsRelease = DiscogsRelease {
    releaseId       :: Integer,
    releaseTitle    :: Text,
    releaseYear     :: Maybe Integer,
    releaseCountry  :: Maybe Text,
    releaseArtists  :: [DiscogsReleaseArtist],
    releaseTracks   :: [DiscogsTrack],
    releaseStyles   :: Maybe [Text],
    releaseGenres   :: [Text],
    releaseMasterId :: Maybe Integer
} deriving (Generic, Show)

instance ToJSON DiscogsRelease where
    toEncoding DiscogsRelease{..} = pairs $
        "id"        .= releaseId       <>
        "title"     .= releaseTitle    <>
        "year"      .= releaseYear     <>
        "country"   .= releaseCountry  <>
        "artists"   .= releaseArtists  <>
        "tracklist" .= releaseTracks   <>
        "styles"    .= releaseStyles   <>
        "genres"    .= releaseGenres   <>
        "master_id" .= releaseMasterId

instance FromJSON DiscogsRelease where
    parseJSON = withObject "DiscogsRelease" $ \v -> do
        releaseId       <- v .:  "id"
        releaseTitle    <- v .:  "title"
        releaseYear     <- v .:? "year"
        releaseCountry  <- v .:? "country"
        releaseArtists  <- v .:  "artists"
        releaseTracks   <- v .:  "tracklist"
        releaseStyles   <- v .:? "styles"
        releaseGenres   <- v .:  "genres"
        releaseMasterId <- v .:? "master_id"
        return DiscogsRelease{..}

--------------------------------------------------------------------------------

data DiscogsMasterRelease = DiscogsMasterRelease {
    masterId          :: Integer,
    masterTitle       :: Text,
    masterYear        :: Integer,
    masterArtists     :: [DiscogsReleaseArtist],
    masterTracks      :: [DiscogsTrack],
    masterStyles      :: Maybe [Text],
    masterGenres      :: [Text],
    masterMainRelease :: Integer
} deriving (Generic, Show)

instance ToJSON DiscogsMasterRelease where
    toEncoding DiscogsMasterRelease{..} = pairs $
        "id"        .= masterId      <>
        "title"     .= masterTitle   <>
        "year"      .= masterYear    <>
        "artists"   .= masterArtists <>
        "tracklist" .= masterTracks  <>
        "styles"    .= masterStyles  <>
        "genres"    .= masterGenres  <>
        "main_release" .= masterMainRelease

instance FromJSON DiscogsMasterRelease where
    parseJSON = withObject "DiscogsMasterRelease" $ \v -> do
        masterId          <- v .:  "id"
        masterTitle       <- v .:  "title"
        masterYear        <- v .:  "year"
        masterArtists     <- v .:  "artists"
        masterTracks      <- v .:  "tracklist"
        masterStyles      <- v .:? "styles"
        masterGenres      <- v .:  "genres"
        masterMainRelease <- v .:  "main_release"
        return DiscogsMasterRelease{..}

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

-- { type: "master" | "release" } ----------------------------------------------

data DiscogsResultType = ResultMaster | ResultRelease deriving (Show, Eq)

instance ToJSON DiscogsResultType where
    toJSON ResultMaster  = AT.String "master"
    toJSON ResultRelease = AT.String "release"

instance FromJSON DiscogsResultType where
    parseJSON = withText "DiscogsResultType" $ \case
        "master"  -> return ResultMaster
        "release" -> return ResultRelease
        other     -> fail $ "expected result type 'master' or 'release', got '" ++ (T.unpack other) ++ "'"

-- DiscogsSearchResult ---------------------------------------------------------

-- TODO (2021-06-27) In TypeScript, I'd implement this as a discriminated union
-- on the "type" field, but I haven't yet found a clean way to express this in Haskell.
-- Failure to fix this means that we might treat a "master" as a "release" and vice-versa.
-- Look into singleton types / data kinds?
data DiscogsSearchResult = DiscogsSearchResult {
    resultId          :: Integer,
    resultGenre       :: [Text],
    resultStyle       :: [Text],
    resultTitle       :: Text,
    resultYear        :: Maybe Text,
    resultLabel       :: [Text],
    resultResourceUrl :: Text,
    resultCountry     :: Maybe Text,
    resultCoverImage  :: Maybe Text,
    resultType        :: DiscogsResultType
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
        "year"         .= resultYear          <> -- TODO year is optional, what do?
        "label"        .= resultLabel         <>
        "resource_url" .= resultResourceUrl   <>
        "country"      .= resultCountry       <>
        "cover_image"  .= resultCoverImage    <>
        "type"         .= resultType

instance FromJSON DiscogsSearchResult where
    parseJSON = withObject "DiscogsSearchResult" $ \v -> do
        resultId          <- v .:  "id"
        resultGenre       <- v .:  "genre"
        resultStyle       <- v .:  "style"
        resultTitle       <- v .:  "title"
        resultYear        <- v .:? "year"
        resultLabel       <- v .:  "label"
        resultResourceUrl <- v .:  "resource_url"
        resultCountry     <- v .:? "country"
        resultCoverImage  <- v .:? "cover_image"
        resultType        <- v .:  "type"
        
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