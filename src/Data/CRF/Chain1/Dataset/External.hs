module Data.CRF.Chain1.Dataset.External
( Word
, Sent
, Dist (unDist)
, mkDist
, WordL
, annotate
, SentL
) where

import           Prelude hiding (Word)

import qualified Data.Set as S
import qualified Data.Map as M

-- | A Word is represented by a set of observations.
type Word a = S.Set a

-- | A sentence of words.
type Sent a = [Word a]

-- | A probability distribution defined over elements of type a.
-- All elements not included in the map have probability equal
-- to 0.
newtype Dist a = Dist { unDist :: M.Map a Double }

-- | Construct the probability distribution.
mkDist :: Ord a => [(a, Double)] -> Dist a
mkDist =
    Dist . normalize . M.fromListWith (+)
  where
    normalize dist =
        let z = sum (M.elems dist)
        in  fmap (/z) dist

-- | A WordL is a labeled word, i.e. a word with probability distribution
-- defined over labels.
type WordL a b = (Word a, Dist b)

-- | Annotate the word with the label.
annotate :: Word a -> b -> WordL a b
annotate w x = (w, Dist (M.singleton x 1))

-- | A sentence of labeled words.
type SentL a b = [WordL a b]
