module Borscht.Util.Fuzzy (
    distanceCosine,
    dotGrams
) where

-- text
import Data.Text (Text)
-- fuzzy set
import qualified Data.FuzzySet.Internal as FZ
import qualified Data.FuzzySet.Util     as FZ
-- data
import Data.HashMap.Strict (HashMap, intersectionWith, elems)

--------------------------------------------------------------------------------

-- computes the dot product of two ngram vectors
dotGrams :: HashMap Text Int -> HashMap Text Int -> Int
dotGrams h1 h2 = sum $ intersectionWith (*) h1 h2

-- TODO (2021-06-26) use FZ.normalized function?
distanceCosine :: Int -> Text -> Text -> Double
distanceCosine n s1 s2 = dot / (FZ.norm (elems g1) * FZ.norm (elems g2))
    where g1 = FZ.gramVector s1 n
          g2 = FZ.gramVector s2 n
          dot = fromIntegral ( dotGrams g1 g2 )
