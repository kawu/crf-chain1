{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}

module Data.CRF.Chain1.Train
( CRF (..)
, train
) where

import Control.Applicative ((<$>), (<*>))
import System.IO (hSetBuffering, stdout, BufferMode (..))
import Data.Binary (Binary, put, get)
-- import qualified Data.Vector as V
import qualified Numeric.SGD as SGD
import qualified Numeric.SGD.LogSigned as L

import Data.CRF.Chain1.Dataset.Internal
import Data.CRF.Chain1.Dataset.External (SentL)
import Data.CRF.Chain1.Dataset.Codec (mkCodec, Codec, encodeDataL)
-- import qualified Data.CRF.Chain1.Dataset.Codec as Codec
import Data.CRF.Chain1.Feature (Feature, featuresIn)
import Data.CRF.Chain1.Model (Model (..), mkModel, FeatIx (..), featToInt)
import Data.CRF.Chain1.Inference (accuracy, expectedFeaturesIn)

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
    -> Bool                         -- ^ Store temporary dataset on a disk
    -> ([(Xs, Ys)] -> [Feature])    -- ^ Feature selection
    -> b                            -- ^ Defaut label
    -> IO [SentL a b]               -- ^ Training data 'IO' action
    -> IO [SentL a b]               -- ^ Evalation data
    -> IO (CRF a b)                 -- ^ Resulting model
train sgdArgs onDisk featSel defLb trainIO evalIO = do
    hSetBuffering stdout NoBuffering

    -- Create codec on the basis of the training dataset
--     (_codec, _trainData) <- mkCodec <$> trainIO
    codec <- mkCodec <$> trainIO
    -- putStr "Codec.obMax: " >> print (Codec.obMax codec)
    -- putStr "Codec.lbMax: " >> print (Codec.lbMax codec)

    -- Encode the training dataset
    trainData_ <- encodeDataL defLb codec <$> trainIO
    SGD.withData onDisk trainData_ $ \trainData -> do
    -- putStrLn "Train dataset ready"

    -- Encode the evaluation dataset
    evalData_ <- encodeDataL defLb codec <$> evalIO
    SGD.withData onDisk evalData_ $ \evalData -> do
    -- putStrLn "Eval dataset ready"

--     evalDataM <- case evalIO'Maybe of
--         Just (x, evalIO) -> Just . encodeDataL x _codec <$> evalIO
--         Nothing -> return Nothing

    -- A set of features
    feats <- featSel <$> SGD.loadData trainData

    -- Train the model
    let crf  = mkModel feats
    para <- SGD.sgd sgdArgs
        (notify sgdArgs crf trainData evalData)
        (gradOn crf) trainData (values crf)
    return $ CRF codec (crf { values = para })


gradOn :: Model -> SGD.Para -> (Xs, Ys) -> SGD.Grad
gradOn crf para (xs, ys) = SGD.fromLogList $
    [ (featToInt curr feat, L.fromPos val)
    | (feat, val) <- featuresIn xs ys ] ++
    [ (ix, L.fromNeg val)
    | (FeatIx ix, val) <- expectedFeaturesIn curr xs ]
  where
    curr = crf { values = para }


notify
    :: SGD.SgdArgs -> Model
    -> SGD.Dataset (Xs, Ys)     -- ^ Training dataset
    -> SGD.Dataset (Xs, Ys)     -- ^ Evaluation dataset
    -> SGD.Para -> Int -> IO ()
notify SGD.SgdArgs{..} model trainData evalData para k
    | doneTotal k == doneTotal (k - 1) = putStr "."
    | SGD.size evalData > 0 = do
        x <- accuracy (model { values = para }) <$> SGD.loadData evalData
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
    trainSize = SGD.size trainData


-- notify
--     :: SGD.SgdArgs -> Model -> [(Xs, Ys)] -> Maybe [(Xs, Ys)]
--     -> SGD.Para -> Int -> IO ()
-- notify SGD.SgdArgs{..} crf trainData evalDataM para k 
--     | doneTotal k == doneTotal (k - 1) = putStr "."
--     | Just dataSet <- evalDataM = do
--         let x = accuracy (crf { values = para }) dataSet
--         putStrLn ("\n" ++ "[" ++ show (doneTotal k) ++ "] acc = " ++ show x)
--     | otherwise =
--         putStrLn ("\n" ++ "[" ++ show (doneTotal k) ++ "] acc = #")
--   where
--     doneTotal :: Int -> Int
--     doneTotal = floor . done
--     done :: Int -> Double
--     done i
--         = fromIntegral (i * batchSize)
--         / fromIntegral trainSize
--     trainSize = length trainData
