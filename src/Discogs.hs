{- HLINT ignore "Redundant bracket" -}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Discogs where

import GHC.Generics
import Data.Text (Text)
import Data.Sequence as Seq

import Data.Aeson
import qualified Data.Aeson.Types as AT

import Text.RawString.QQ
import qualified Data.ByteString.Lazy.Char8 as BS

--------------------------------------------------------------------------------

data DiscogsRelease = DiscogsRelease {
    releaseId :: Integer,
    releaseTitle :: Text
} deriving (Generic, Show)

instance ToJSON DiscogsRelease where
    -- No need to provide a toJSON implementation.  For efficiency, we write a
    -- simple toEncoding implementation, as the default version uses toJSON.
    --toEncoding = genericToEncoding defaultOptions
    toEncoding DiscogsRelease{..} = pairs $
        "id"        .= releaseId         <>
        "title"     .= releaseTitle

instance FromJSON DiscogsRelease where
    parseJSON = withObject "DiscogsRelease" $ \v -> do
        releaseId       <- v .: "id"
        releaseTitle    <- v .: "title"
        return DiscogsRelease{..}

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

searchResultJSON :: BS.ByteString
searchResultJSON = [r|{ "results" : [
    {
        "catno": "87",
        "uri": "/Mazouni-Ecoute-Moi-Camarade-Achhal-Dabart-Alik/master/1737699",
        "master_id": 1737699,
        "thumb": "https://img.discogs.com/TT7giSjuASwNwakqvUhhXzonvw0=/fit-in/150x150/filters:strip_icc():format(jpeg):mode_rgb():quality(40)/discogs-images/R-8288737-1458700348-8134.jpeg.jpg",
        "genre": ["Folk, World, & Country"],
        "format": ["Vinyl","7\"","45 RPM"],
        "community": {"have": 6,"want": 375},
        "title": "Mazouni* - Ecoute Moi Camarade / Achhal Dabart Alik",
        "cover_image": "https://img.discogs.com/2iWwflGEgPrn7MnsojaZ36M2_yI=/fit-in/600x600/filters:strip_icc():format(jpeg):mode_rgb():quality(90)/discogs-images/R-8288737-1458700348-8134.jpeg.jpg",
        "id": 1737699,
        "year": "1974",
        "style": [],
        "barcode": [],
        "user_data": {
            "in_collection": false,
            "in_wantlist": false
        },
        "label": ["DDA", "Cheniki"],
        "resource_url": "https://api.discogs.com/masters/1737699",
        "country": "Algeria",
        "master_url": "https://api.discogs.com/masters/1737699",
        "type": "master"
    }
] }
|]

runParseTest :: IO ()
runParseTest = do
    putStrLn "runParseTest\n\n"
    putStrLn "\n\n\n\ndecoding...\n\n"
    let x = eitherDecode searchResultJSON :: Either String DiscogsSearchResults
    print x
    print $ encode x
    BS.putStrLn $ encode (Seq.fromList [1,2,3::Int])