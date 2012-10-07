module Data.CRF
(
-- * External data representation
  Word
, Sent
, Dist
, WordL
, SentL
, mkDist
, annotate

-- * Internal data representation

-- * Encoding and decoding (and codec)
, Codec
, mkCodec
, encodeData
, encodeDataL
, encodeSent
, encodeSentL
, decodeLabel
, decodeLabels

-- * Feature data type and extraction
, Feature (..)
, featuresIn
, hiddenFeats
, hiddenOFeats
, hiddenTFeats
, hiddenSFeats
, presentFeats
, presentOFeats
, presentTFeats
, presentSFeats

-- * Training the model
, Model
, train

-- * Inference with the model
, tag
, accuracy
) where

import Data.CRF.External
import Data.CRF.Codec
import Data.CRF.Feature
import Data.CRF.Feature.Present
import Data.CRF.Feature.Hidden
import Data.CRF.Train
import Data.CRF.Inference
