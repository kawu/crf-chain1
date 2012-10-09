{-# LANGUAGE RecordWildCards #-}

-- | The module provides data types and manipulation functions for the
-- first-order, linear-chain conditional random fields.

module Data.CRF
(
-- * Data types
  Word
, Sent
, Dist (unDist)
, mkDist
, WordL
, annotate
, SentL

-- * CRF
, CRF (..)
-- ** Training
, train
-- ** Tagging
, tag

-- * Feature selection
, hiddenFeats
, presentFeats
) where

import Data.CRF.Dataset.External
import Data.CRF.Dataset.Codec
import Data.CRF.Feature.Present
import Data.CRF.Feature.Hidden
import Data.CRF.Train
import qualified Data.CRF.Inference as I

-- | Determine the most probable label sequence within the context of the
-- given sentence using the model provided by the 'CRF'.
tag :: (Ord a, Ord b) => CRF a b -> Sent a -> [b]
tag CRF{..} = decodeLabels codec . I.tag model . encodeSent codec
