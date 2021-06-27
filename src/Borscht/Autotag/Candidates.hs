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
        tcTitle    :: NormalizedText,
        tcArtists  :: [ArtistCandidate],
        tcYear     :: Maybe Integer,
        tcCountry  :: Maybe Text,
        tcDuration :: Maybe Integer
    } deriving (Show)