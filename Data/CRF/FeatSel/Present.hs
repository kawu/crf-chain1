module Data.CRF.FeatSel.Present
( presentFeats
, presentOFeats
, presentTFeats
, presentSFeats
) where

import qualified Data.Vector as V

import Data.CRF.Core
import Data.CRF.Feature

presentOFeats :: Dataset X Y -> [Feature]
presentOFeats ds =
    concatMap sentOFeats $ V.toList ds
  where
    sentOFeats (xs, ys) = concatMap oFeatsOn (zip (V.toList xs) (V.toList ys))
    oFeatsOn (x, choice) =
        [ OFeature o y
        | o <- unX x
        , y <- lbs choice ]

presentTFeats :: Dataset x Y -> [Feature]
presentTFeats ds =
    concatMap (sentTFeats.snd) (V.toList ds)
  where
    sentTFeats ys = concatMap (tFeatsOn ys) [1 .. V.length ys - 1]
    tFeatsOn ys k =
        [ TFeature x y
        | x <- lbs (ys V.! k)
        , y <- lbs (ys V.! (k-1)) ]

presentSFeats :: Dataset x Y -> [Feature]
presentSFeats ds =
    let sentSFeats s = [SFeature x | x <- lbs (s V.! 0)] 
    in  concatMap (sentSFeats.snd) $ V.toList ds

presentFeats :: Dataset X Y -> [Feature]
presentFeats ds
    =  presentOFeats ds
    ++ presentTFeats ds
    ++ presentSFeats ds

lbs :: Y -> [Lb]
lbs = map fst . unY
