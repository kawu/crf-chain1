-- | The module provides feature selection functions which extract
-- features present in the dataset, i.e. features which directly occure
-- the dataset.
--
-- You can mix functions defined here with the selection functions
-- from the "Data.CRF.Chain1.Feature.Hidden" module.

module Data.CRF.Feature.Present
( presentFeats
, presentOFeats
, presentTFeats
, presentSFeats
) where

import qualified Data.Vector as V

import Data.CRF.Dataset.Internal
import Data.CRF.Feature

-- | 'OFeature's which occur in the dataset.
presentOFeats :: [(Xs, Ys)] -> [Feature]
presentOFeats ds =
    concatMap sentOFeats ds
  where
    sentOFeats (xs, ys) = concatMap oFeatsOn (zip (V.toList xs) (V.toList ys))
    oFeatsOn (x, choice) =
        [ OFeature o y
        | o <- unX x
        , y <- lbs choice ]

-- | 'TFeature's which occur in the dataset.
presentTFeats :: [(a, Ys)] -> [Feature]
presentTFeats ds =
    concatMap (sentTFeats.snd) ds
  where
    sentTFeats ys = concatMap (tFeatsOn ys) [1 .. V.length ys - 1]
    tFeatsOn ys k =
        [ TFeature x y
        | x <- lbs (ys V.! k)
        , y <- lbs (ys V.! (k-1)) ]

-- | 'SFeature's which occur in the dataset.
presentSFeats :: [(a, Ys)] -> [Feature]
presentSFeats ds =
    let sentSFeats s = [SFeature x | x <- lbs (s V.! 0)] 
    in  concatMap (sentSFeats.snd) ds

-- | 'Feature's of all kinds which occur in the dataset.
presentFeats :: [(Xs, Ys)] -> [Feature]
presentFeats ds
    =  presentOFeats ds
    ++ presentTFeats ds
    ++ presentSFeats ds

lbs :: Y -> [Lb]
lbs = map fst . unY
