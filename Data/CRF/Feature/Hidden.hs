module Data.CRF.Feature.Hidden
( hiddenFeats
, hiddenOFeats
, hiddenTFeats
, hiddenSFeats
) where

import qualified Data.Set as S
import qualified Data.Vector as V

import Data.CRF.Core
import Data.CRF.Feature

hiddenOFeats :: [(Xs, Ys)] -> [Feature]
hiddenOFeats ds =
    [ OFeature o x
    | o <- collectObs ds
    , x <- collectLbs ds ]

hiddenTFeats :: [(Xs, Ys)] -> [Feature]
hiddenTFeats ds =
    let xs = collectLbs ds
    in  [TFeature x y | x <- xs, y <- xs]

hiddenSFeats :: [(Xs, Ys)] -> [Feature]
hiddenSFeats = map SFeature . collectLbs

hiddenFeats :: [(Xs, Ys)] -> [Feature]
hiddenFeats ds
    =  hiddenOFeats ds
    ++ hiddenTFeats ds
    ++ hiddenSFeats ds

collectObs :: [(Xs, Ys)] -> [Ob]
collectObs = nub . concatMap (sentObs . fst)

collectLbs :: [(Xs, Ys)] -> [Lb]
collectLbs = nub . concatMap (sentLbs . snd)

sentObs :: Xs -> [Ob]
sentObs = concatMap unX . V.toList

sentLbs :: Ys -> [Lb]
sentLbs = concatMap lbsAll . V.toList

lbsAll :: Y -> [Lb]
lbsAll = map fst . unY

nub :: Ord a => [a] -> [a]
nub = S.toList . S.fromList
