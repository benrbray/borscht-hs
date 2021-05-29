module Fuzzy where

-- text
import Data.Text (Text, toLower)
import qualified Data.Text as Text

-- unicode normalization
import Data.Text.ICU.Char
import Data.Text.ICU.Normalize
import Data.Text.ICU.Translit (trans, transliterate)

-- fuzzy set
import qualified Data.FuzzySet as FZ
import qualified Data.FuzzySet.Types as FZ
import qualified Data.FuzzySet.Internal as FZ
import qualified Data.FuzzySet.Util as FZ

-- data structures
import Data.List
import Data.HashMap.Strict (HashMap, foldrWithKey, alter, intersectionWith, elems)

--------------------------------------------------------------------------------

-- | remove accents (see [StackOverflow](https://stackoverflow.com/a/44290219/1444650))
noAccents :: Text -> Text
noAccents text = Text.filter (not . property Diacritic) normalizedText
  where
    normalizedText = normalize NFD text

-- | Read about [Transliterator Identifiers](https://unicode-org.github.io/icu/userguide/transforms/general/#transliterator-identifiers)
-- TODO kanji are interpreted as chinese ; need to use external library like KAKASI
toLatin :: Text -> Text
toLatin = transliterate (trans "NFD; Latin; Lower")

-- | Normalize the input by
--   * removing accents
--   * removing non-word characters, except for spaces and commas; and
--   * converting alphabetic characters to lowercase.
normalized :: Text -> Text
normalized = noAccents . toLatin

distanceCosine :: Int -> Text -> Text -> Double
distanceCosine n s1 s2 = dot / (FZ.norm (elems g1) * FZ.norm (elems g2))
    where g1 = FZ.gramVector (FZ.normalized s1) n
          g2 = FZ.gramVector (FZ.normalized s2) n
          dot = fromIntegral ( dotGrams g1 g2 )

-- computes the dot product of two gram vectors
dotGrams :: HashMap Text Int -> HashMap Text Int -> Int
dotGrams h1 h2 = sum $ intersectionWith (*) h1 h2