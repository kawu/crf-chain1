module Data.CRF.Feature
( Feature (..)
, isSFeat
, isTFeat
, isOFeat
, featuresIn
) where

import Data.Binary (Binary, Get, put, get)
import Control.Applicative ((<*>), (<$>))
import qualified Data.Vector as V
import qualified Data.Number.LogFloat as L

import Data.CRF.Core

-- | TFeature x y:
--   * x is a label corresponding to the current position,
--   * y is a label corresponding to the previous position.
--   OFeature o x:
--   * o is an observation corresponding to the current position,
--   * x is a label corresponding to the current position.
data Feature
    = SFeature {-# UNPACK #-} !Lb
    | TFeature {-# UNPACK #-} !Lb {-# UNPACK #-} !Lb
    | OFeature {-# UNPACK #-} !Ob {-# UNPACK #-} !Lb
    deriving (Show, Read, Eq, Ord)

instance Binary Feature where
    put (SFeature x)   = put (0 :: Int) >> put x
    put (TFeature x y) = put (1 :: Int) >> put (x, y)
    put (OFeature o x) = put (2 :: Int) >> put (o, x)
    get = do
        k <- get :: Get Int
        case k of
            0 -> SFeature <$> get
            1 -> TFeature <$> get <*> get
            2 -> OFeature <$> get <*> get
	    _ -> error "Binary Feature: unknown identifier"

isSFeat :: Feature -> Bool
isSFeat (SFeature _) = True
isSFeat _            = False

isOFeat :: Feature -> Bool
isOFeat (OFeature _ _) = True
isOFeat _              = False

isTFeat :: Feature -> Bool
isTFeat (TFeature _ _) = True
isTFeat _              = False

-- | Features present in data together with corresponding probabilities.
--   TODO: consider doing computation in log scale (would have to change
--   Model.Internal.updateWithNumbers too).

-- | Transition features with assigned probabilities for given position.
trFeats :: Ys -> Int -> [(Feature, L.LogFloat)]
trFeats ys 0 =
    [ (SFeature x, L.logFloat px)
    | (x, px) <- unY (ys V.! 0) ]
trFeats ys k =
    [ (TFeature x y, L.logFloat px * L.logFloat py)
    | (x, px) <- unY (ys V.! k)
    , (y, py) <- unY (ys V.! (k-1)) ]

-- | Observation features with assigned probabilities for a given position.
obFeats :: Xs -> Ys -> Int -> [(Feature, L.LogFloat)]
obFeats xs ys k =
    [ (OFeature o x, L.logFloat px)
    | (x, px) <- unY (ys V.! k)
    , o       <- unX (xs V.! k) ]

-- | All features with assigned probabilities for given position.
features :: Xs -> Ys -> Int -> [(Feature, L.LogFloat)]
features xs ys k = trFeats ys k ++ obFeats xs ys k

-- | All features with assigned probabilities in given sentence.
featuresIn :: Xs -> Ys -> [(Feature, L.LogFloat)]
featuresIn xs ys = concatMap (features xs ys) [0 .. V.length xs - 1]
