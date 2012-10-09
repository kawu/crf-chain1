-- | The module provides feature selection functions which extract
-- hidden features, i.e. all features which can be constructed 
-- (by means of cartesian product) on the basis of the set of
-- observations and the set of labels.
-- For example, the list of hidden observation features can
-- be defined as 'OFeature' '<$>' os '<*>' xs, where os is a
-- list of all observations and xs is a list of all labels.
--
-- You can mix functions defined here with the selection functions
-- from the "Data.CRF.Chain1.Feature.Present" module.

module Data.CRF.Chain1.Feature.Hidden
( hiddenFeats
, hiddenOFeats
, hiddenTFeats
, hiddenSFeats
) where

import qualified Data.Set as S
import qualified Data.Vector as V

import Data.CRF.Chain1.Dataset.Internal
import Data.CRF.Chain1.Feature

-- | Hidden 'OFeature's which can be constructed based on the dataset.
hiddenOFeats :: [(Xs, Ys)] -> [Feature]
hiddenOFeats ds =
    [ OFeature o x
    | o <- collectObs ds
    , x <- collectLbs ds ]

-- | Hidden 'TFeature's which can be constructed based on the dataset.
hiddenTFeats :: [(Xs, Ys)] -> [Feature]
hiddenTFeats ds =
    let xs = collectLbs ds
    in  [TFeature x y | x <- xs, y <- xs]

-- | Hidden 'SFeature's which can be constructed based on the dataset.
hiddenSFeats :: [(Xs, Ys)] -> [Feature]
hiddenSFeats = map SFeature . collectLbs

-- | Hidden 'Feature's of all types which can be constructed
-- based on the dataset.
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
