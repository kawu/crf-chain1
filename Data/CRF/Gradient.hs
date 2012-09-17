module Data.CRF.Gradient
( mkGrad
) where

import qualified Numeric.SGD as SGD
import qualified Numeric.SGD.LogSigned as L

import Data.CRF.Core
import Data.CRF.Feature (featuresIn)
import Data.CRF.Model (Model (..), featToInt)
import Data.CRF.Inference (expectedFeaturesIn)

mkGrad :: Model -> SGD.Para -> (Xs, Ys) -> SGD.Grad
mkGrad crf para (xs, ys) = SGD.fromLogList $
    [ (featToInt curr feat, L.fromPos val)
    | (feat, val) <- featuresIn xs ys ] ++
    [ (ix, L.fromNeg val)
    | (FeatIx ix, val) <- expectedFeaturesIn curr xs ]
  where
    curr = crf { values = para }

-- instance DataElem Model (Sent X, Sent Y) where  
-- 
--     computeGrad crf part buffer =
--         let ns = concatMap (uncurry featuresIn) part
--             ens = concatMap (expectedFeaturesIn crf . fst) part
--             followPtrs = map $ \(feat, val) -> (featToIx crf feat, val)
--         in do
--             gradient <- MA.consumeWith logAdd ens buffer
--                     >>= MA.mapArray (\v -> - exp v) 
--                     >>= MA.consumeWith (+) (followPtrs ns)
--             return gradient
