module Borscht.Autotag.Candidates where

-- text
import Data.Text (Text)
import Borscht.Util.Normalize (NormalizedText)

------------------------------------------------------------

data ArtistCandidate = ArtistCandidate {
        acId :: Integer,
        acName :: NormalizedText
    } deriving (Show)

data TrackCandidate = TrackCandidate {
        -- album / release information
        tcReleaseTitle :: Maybe NormalizedText,
        tcDgsReleaseId :: Maybe Integer,
        tcMbzReleaseId :: Maybe Integer,
        -- track information
        tcTitle        :: NormalizedText,
        tcArtists      :: [ArtistCandidate],
        tcYear         :: Maybe Integer,
        tcGenres       :: [Text],
        tcCountry      :: Maybe Text,
        tcDuration     :: Maybe Integer
    } deriving (Show)