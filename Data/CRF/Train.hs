{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}

module Data.CRF.Train
( CRF (..)
, train
) where

import Control.Applicative ((<$>), (<*>))
import System.IO (hSetBuffering, stdout, BufferMode (..))
import Data.Binary (Binary, put, get)
import qualified Data.Vector as V
import qualified Numeric.SGD as SGD
import qualified Numeric.SGD.LogSigned as L

import Data.CRF.Dataset.Internal
import Data.CRF.Dataset.External (SentL)
import Data.CRF.Dataset.Codec (mkCodec, Codec, encodeDataL)
import Data.CRF.Feature (Feature, featuresIn)
import Data.CRF.Model (Model (..), mkModel, FeatIx (..), featToInt)
import Data.CRF.Inference (accuracy, expectedFeaturesIn)

-- | A conditional random field model with additional codec used for
-- data encoding.
data CRF a b = CRF {
    -- | The codec is used to transform data into internal representation,
    -- where each observation and each label is represented by a unique
    -- integer number.
    codec :: Codec a b,
    -- | The actual model, which is a map from 'Feature's to potentials.
    model :: Model }

instance (Ord a, Ord b, Binary a, Binary b) => Binary (CRF a b) where
    put CRF{..} = put codec >> put model
    get = CRF <$> get <*> get

-- | Train the CRF using the stochastic gradient descent method.
-- The resulting model will contain features extracted with
-- the user supplied extraction function.
-- You can use the functions provided by the "Data.CRF.Chain1.Feature.Present"
-- and "Data.CRF.Chain1.Feature.Hidden" modules for this purpose.
-- When the evaluation data 'IO' action is 'Just', the iterative
-- training process will notify the user about the current accuracy
-- on the evaluation part every full iteration over the training part.
train
    :: (Ord a, Ord b)
    => SGD.SgdArgs                  -- ^ Args for SGD
    -> IO [SentL a b]               -- ^ Training data 'IO' action
    -> Maybe (b, IO [SentL a b])    -- ^ Default label and evalation data
    -> ([(Xs, Ys)] -> [Feature])    -- ^ Feature selection
    -> IO (CRF a b)                 -- ^ Resulting model
train sgdArgs trainIO evalIO'Maybe extractFeats = do
    hSetBuffering stdout NoBuffering
    (_codec, trainData) <- mkCodec <$> trainIO
    evalDataM <- case evalIO'Maybe of
        Just (x, evalIO) -> Just . encodeDataL x _codec <$> evalIO
        Nothing -> return Nothing
    let crf  = mkModel (extractFeats trainData)
    para <- SGD.sgdM sgdArgs
        (notify sgdArgs crf trainData evalDataM)
        (gradOn crf) (V.fromList trainData) (values crf)
    return $ CRF _codec (crf { values = para })

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
