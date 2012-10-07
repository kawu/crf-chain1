{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}

module Data.CRF.Train
( CRF
, train
) where

import Control.Applicative ((<$>))
import System.IO (hSetBuffering, stdout, BufferMode (..))
import qualified Data.Vector as V
import qualified Numeric.SGD as SGD
import qualified Numeric.SGD.LogSigned as L

import Data.CRF.Core
import Data.CRF.External (SentL)
import Data.CRF.Feature (Feature)
import Data.CRF.Codec (mkCodec, Codec, encodeDataL)
import Data.CRF.Feature (featuresIn)
import Data.CRF.Model (Model (..), mkModel, featToInt)
import Data.CRF.Inference (accuracy, expectedFeaturesIn)

type CRF a b = (Codec a b, Model)

-- | Train the CRF using the stochastic gradient descent method.
-- The resulting model will contain features extracted with
-- the user supplied extraction function.  When the evaluation
-- data 'IO' action is 'Just', the iterative training process
-- will notify the user about the current accuracy on the
-- evaluation part every full iteration over the training part.
train
    :: (Ord a, Ord b)
    => SGD.SgdArgs                  -- ^ Args for SGD
    -> IO [SentL a b]               -- ^ Training data 'IO' action
    -> Maybe (b, IO [SentL a b])    -- ^ Default label and evalation data
    -> ([(Xs, Ys)] -> [Feature])    -- ^ Feature extraction
    -> IO (CRF a b)                 -- ^ Resulting model
train sgdArgs trainIO evalIO'Maybe extractFeats = do
    hSetBuffering stdout NoBuffering
    (codec, trainData) <- mkCodec <$> trainIO
    evalDataM <- case evalIO'Maybe of
        Just (x, evalIO) -> Just . encodeDataL x codec <$> evalIO
        Nothing -> return Nothing
    let crf  = mkModel (extractFeats trainData)
    para <- SGD.sgdM sgdArgs
        (notify sgdArgs crf trainData evalDataM)
        (gradOn crf) (V.fromList trainData) (values crf)
    return (codec, crf { values = para })

gradOn :: Model -> SGD.Para -> (Xs, Ys) -> SGD.Grad
gradOn crf para (xs, ys) = SGD.fromLogList $
    [ (featToInt curr feat, L.fromPos val)
    | (feat, val) <- featuresIn xs ys ] ++
    [ (ix, L.fromNeg val)
    | (FeatIx ix, val) <- expectedFeaturesIn curr xs ]
  where
    curr = crf { values = para }

notify
    :: SGD.SgdArgs -> Model -> [(Xs, Ys)] -> Maybe [(Xs, Ys)]
    -> SGD.Para -> Int -> IO ()
notify SGD.SgdArgs{..} crf trainData evalDataM para k 
    | doneTotal k == doneTotal (k - 1) = putStr "."
    | Just dataSet <- evalDataM = do
        let x = accuracy (crf { values = para }) dataSet
        putStrLn ("\n" ++ "[" ++ show (doneTotal k) ++ "] f = " ++ show x)
    | otherwise =
        putStrLn ("\n" ++ "[" ++ show (doneTotal k) ++ "] f = #")
  where
    doneTotal :: Int -> Int
    doneTotal = floor . done
    done :: Int -> Double
    done i
        = fromIntegral (i * batchSize)
        / fromIntegral trainSize
    trainSize = length trainData
