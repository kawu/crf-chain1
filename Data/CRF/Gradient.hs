module Data.CRF.Gradient
(
) where

import qualified Numeric.SGD as SGD
import qualified Numeric.SGD.Grad as SGD
import qualified Numeric.SGD.LogSigned as L

import Data.CRF.Core

-- import Data.CRF.LogMath (logAdd)
import Data.CRF.Feature (featuresIn)
import Data.CRF.CRF.Model (Model, featToIx)
import Data.CRF.CRF.Infere (expectedFeaturesIn)
import qualified Data.CRF.CRF.Infere as CRF

instance DataElem Model (Sent X, Sent Y) where  

    computeGrad crf part buffer =
        let ns = concatMap (uncurry featuresIn) part
            ens = concatMap (expectedFeaturesIn crf . fst) part
            followPtrs = map $ \(feat, val) -> (featToIx crf feat, val)
        in do
            gradient <- MA.consumeWith logAdd ens buffer
                    >>= MA.mapArray (\v -> - exp v) 
                    >>= MA.consumeWith (+) (followPtrs ns)
            return gradient

    accuracy = CRF.accuracy 
