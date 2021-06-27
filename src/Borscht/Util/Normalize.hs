module Borscht.Util.Normalize (
    NormalizedText(textNormal, textOriginal),   -- intentionally do not export constructor
    mkNormalizedText, -- smart constructor
    normalized
) where

-- text
import Data.Text (Text)
import qualified Data.Text as Text

-- unicode normalization
import Data.Text.ICU.Char
import Data.Text.ICU.Normalize
import Data.Text.ICU.Translit (trans, transliterate)

--------------------------------------------------------------------------------

data NormalizedText = NormalizedText {
    textOriginal :: Text,
    textNormal   :: Text
  } deriving (Show)

-- ensures second value is normalized version of the first
mkNormalizedText :: Text -> NormalizedText
mkNormalizedText t = NormalizedText t (normalized t)

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
