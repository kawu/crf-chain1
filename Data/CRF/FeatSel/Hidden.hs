module Data.CRF.FeatSel.Hidden
( hiddenFeats
, hiddenOFeats
, hiddenTFeats
, hiddenSFeats
) where

import qualified Data.Set as S
import qualified Data.Vector as V

import Data.CRF.Core
import Data.CRF.Feature

hiddenOFeats :: Dataset X Y -> [Feature]
hiddenOFeats ds =
    [ OFeature o x
    | o <- collectObs ds
    , x <- collectLbs ds ]

hiddenTFeats :: Dataset X Y -> [Feature]
hiddenTFeats ds =
    let xs = collectLbs ds
    in  [TFeature x y | x <- xs, y <- xs]

hiddenSFeats :: Dataset X Y -> [Feature]
hiddenSFeats = map SFeature . collectLbs

hiddenFeats :: Dataset X Y -> [Feature]
hiddenFeats ds
    =  hiddenOFeats ds
    ++ hiddenTFeats ds
    ++ hiddenSFeats ds

collectObs :: Dataset X Y -> [Ob]
collectObs = nub . concatMap (sentObs . fst) . V.toList

collectLbs :: Dataset X Y -> [Lb]
collectLbs = nub . concatMap (sentLbs . snd) . V.toList

sentObs :: Xs -> [Ob]
sentObs = concatMap unX . V.toList

sentLbs :: Ys -> [Lb]
sentLbs = concatMap lbsAll . V.toList

lbsAll :: Y -> [Lb]
lbsAll = map fst . unY

nub :: Ord a => [a] -> [a]
nub = S.toList . S.fromList
