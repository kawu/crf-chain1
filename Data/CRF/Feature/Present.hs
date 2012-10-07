module Data.CRF.Feature.Present
( presentFeats
, presentOFeats
, presentTFeats
, presentSFeats
) where

import qualified Data.Vector as V

import Data.CRF.Core
import Data.CRF.Feature

presentOFeats :: [(Xs, Ys)] -> [Feature]
presentOFeats ds =
    concatMap sentOFeats ds
  where
    sentOFeats (xs, ys) = concatMap oFeatsOn (zip (V.toList xs) (V.toList ys))
    oFeatsOn (x, choice) =
        [ OFeature o y
        | o <- unX x
        , y <- lbs choice ]

presentTFeats :: [(a, Ys)] -> [Feature]
presentTFeats ds =
    concatMap (sentTFeats.snd) ds
  where
    sentTFeats ys = concatMap (tFeatsOn ys) [1 .. V.length ys - 1]
    tFeatsOn ys k =
        [ TFeature x y
        | x <- lbs (ys V.! k)
        , y <- lbs (ys V.! (k-1)) ]

presentSFeats :: [(a, Ys)] -> [Feature]
presentSFeats ds =
    let sentSFeats s = [SFeature x | x <- lbs (s V.! 0)] 
    in  concatMap (sentSFeats.snd) ds

presentFeats :: [(Xs, Ys)] -> [Feature]
presentFeats ds
    =  presentOFeats ds
    ++ presentTFeats ds
    ++ presentSFeats ds

lbs :: Y -> [Lb]
lbs = map fst . unY
