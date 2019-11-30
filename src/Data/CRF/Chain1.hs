{-# LANGUAGE RecordWildCards #-}

-- | The module provides first-order, linear-chain conditional random fields
-- (CRFs).
--
-- Important feature of the implemented flavour of CRFs is that transition
-- features which are not included in the CRF model are considered to have
-- probability of 0. 
-- It is particularly useful when the training material determines the set
-- of possible label transitions (e.g. when using the IOB encoding method).
-- Furthermore, this design decision makes the implementation much faster
-- for sparse datasets.

module Data.CRF.Chain1
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

import Prelude hiding (Word)

import Data.CRF.Chain1.Dataset.External
import Data.CRF.Chain1.Dataset.Codec
import Data.CRF.Chain1.Feature.Present
import Data.CRF.Chain1.Feature.Hidden
import Data.CRF.Chain1.Train
import qualified Data.CRF.Chain1.Inference as I

-- | Determine the most probable label sequence within the context of the
-- given sentence using the model provided by the 'CRF'.
tag :: (Ord a, Ord b) => CRF a b -> Sent a -> [b]
tag CRF{..} = decodeLabels codec . I.tag model . encodeSent codec
