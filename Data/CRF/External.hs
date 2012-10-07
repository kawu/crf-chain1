module Data.CRF.External
( Word
, Sent
, Dist
, mkDist
, WordL
, SentL
, annotate
) where

import qualified Data.Set as S
import qualified Data.Map as M

-- | A Word is represented by a set of observations.
type Word a = S.Set a

-- | A sentence of words.
type Sent a = [Word a]

-- | A probability distribution defined over elements of type a.
-- All elements not included in the map have probability equall
-- to 0.
type Dist a = M.Map a Double

-- | Make the probability distribution.
mkDist :: Ord a => [(a, Double)] -> Dist a
mkDist =
    normalize . M.fromListWith (+)
  where
    normalize dist =
        let z = sum (M.elems dist)
        in  fmap (/z) dist

-- | A WordL is a labeled word, i.e. a word with probability distribution
-- defined over labels.
type WordL a b = (Word a, Dist b)

-- | A sentence of labeled words.
type SentL a b = [WordL a b]

-- | Annotate the word with the label.
annotate :: b -> Word a -> WordL a b
annotate x w = (w, M.singleton x 1)
