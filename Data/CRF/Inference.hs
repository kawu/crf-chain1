{-# LANGUAGE FlexibleContexts #-}

-- Inference with CRFs.  The assumption is made, that probability of 0
-- (-Infinity potential) is assigned to all transitions which are not
-- a part of the CRF model.

module Data.CRF.Inference
( module Data.CRF.Model
, tag
, accuracy
, expectedFeaturesIn
, zx
, zx'
) where

import Data.List (maximumBy)
import Data.Function (on)
import qualified Data.Array as A
import qualified Data.Vector as V

import Control.Parallel.Strategies (rseq, parMap)
import Control.Parallel (par, pseq)
import GHC.Conc (numCapabilities)
import qualified Data.Number.LogFloat as L

import qualified Data.CRF.DP as DP
import Data.CRF.Util (partition)
import Data.CRF.Core
import Data.CRF.Model

type ProbArray = Int -> Lb -> L.LogFloat
type AccF = [L.LogFloat] -> L.LogFloat

-- | General model methods.

computePsi :: Model -> Xs -> Int -> Lb -> L.LogFloat
computePsi crf xs i = (A.!) $ A.accumArray (*) 1 bounds
    [ (x, valueL crf ix)
    | ob <- unX (xs V.! i)
    , (x, ix) <- obIxs crf ob ]
  where
    bounds = (Lb 0, Lb $ lbNum crf - 1)

-- | Forward table computation.
forward :: AccF -> Model -> Xs -> ProbArray
forward acc crf sent = DP.flexible2
    (0, V.length sent) wordBounds
    (\t k -> withMem (computePsi crf sent k) t k)
  where
    wordBounds k
        | k == V.length sent = (Lb 0, Lb 0)
        | otherwise = (Lb 0, Lb $ lbNum crf - 1)
    -- | Forward table equation, where k is current position, x is a label
    -- on current position and psi is a psi table computed for current
    -- position.
    -- FIXME: null sentence?
    withMem psi alpha k x
        | k == 0 = psi x * sgValue crf x 
        | k == V.length sent = acc
            [ alpha (k - 1) y
            | y <- lbSet crf ]
        | otherwise = acc
            [ alpha (k - 1) y * psi x * valueL crf ix
            | (y, ix) <- prevIxs crf x ]

-- | Backward table computation.
backward :: AccF -> Model -> Xs -> ProbArray
backward acc crf sent = DP.flexible2
    (0, V.length sent) wordBounds
    (\t k -> withMem (computePsi crf sent k) t k)
  where
    wordBounds k
        | k == 0    = (Lb 0, Lb 0)
        | otherwise = (Lb 0, Lb $ lbNum crf - 1)
    -- | Backward table equation, where k is current position, y is a label
    -- on previous, k-1, position and psi is a psi table computed for current
    -- position.
    withMem psi beta k y
        | k == V.length sent = 1
        | k == 0    = acc
            [ beta (k + 1) x * psi x * valueL crf ix
            | (x, ix) <- sgIxs crf ]
        | otherwise = acc
            [ beta (k + 1) x * psi x * valueL crf ix
            | (x, ix) <- nextIxs crf y ]

zxBeta :: ProbArray -> L.LogFloat
zxBeta beta = beta 0 0

zxAlpha :: Xs -> ProbArray -> L.LogFloat
zxAlpha sent alpha = alpha (V.length sent) 0

zx :: Model -> Xs -> L.LogFloat
zx crf = zxBeta . backward sum crf

zx' :: Model -> Xs -> L.LogFloat
zx' crf sent = zxAlpha sent (forward sum crf sent)

--------------------------------------------------------------
argmax :: Ord b => (a -> b) -> [a] -> (a, b)
argmax _ [] = error "argmax: null list"
argmax f xs =
    foldl1 choice $ map (\x -> (x, f x)) xs
  where
    choice (x1, v1) (x2, v2)
        | v1 > v2 = (x1, v1)
        | otherwise = (x2, v2)

dynamicTag :: Model -> Xs -> [Lb]
dynamicTag crf sent = collectMaxArg (0, 0) [] $ DP.flexible2
    (0, V.length sent) wordBounds
    (\t k -> withMem (computePsi crf sent k) t k)
  where
    wordBounds k
        | k == 0    = (Lb 0, Lb 0)
        | otherwise = (Lb 0, Lb $ lbNum crf - 1)

    withMem psi mem k y
        | k == V.length sent = (-1, 1)
        | k == 0    = prune . argmax eval $ sgIxs crf
        | otherwise = prune . argmax eval $ nextIxs crf y
      where
        eval (x, ix) = (snd $ mem (k + 1) x) * psi x * valueL crf ix
        prune ((x, _ix), v) = (x, v)

    collectMaxArg (i, j) acc mem =
        collect (mem i j)
      where
        collect (h, _)
            | h == -1   = reverse acc
            | otherwise = collectMaxArg (i + 1, h) (h:acc) mem

tag :: Model -> Xs -> [Lb]
tag = dynamicTag
{-# INLINE tag #-}

-- tagProbs :: Sent s => Model -> s -> [[Double]]
-- tagProbs crf sent =
--     let alpha = forward maximum crf sent
--         beta = backward maximum crf sent
--         normalize vs =
--             let d = - logSum vs
--             in map (+d) vs
--         m1 k x = alpha k x + beta (k + 1) x
--     in  [ map exp $ normalize [m1 i k | k <- interpIxs sent i]
--         | i <- [0 .. V.length sent - 1] ]
-- 
-- -- tag probabilities with respect to
-- -- marginal distributions
-- tagProbs' :: Sent s => Model -> s -> [[Double]]
-- tagProbs' crf sent =
--     let alpha = forward logSum crf sent
--         beta = backward logSum crf sent
--     in  [ [ exp $ prob1 crf alpha beta sent i k
--           | k <- interpIxs sent i ]
--         | i <- [0 .. V.length sent - 1] ]

goodAndBad :: Model -> Xs -> Ys -> (Int, Int)
goodAndBad crf sent labels =
    foldl gather (0, 0) (zip labels' labels'')
  where
    labels' = [ fst . maximumBy (compare `on` snd) $ unY (labels V.! i)
              | i <- [0 .. V.length labels - 1] ]
    labels'' = tag crf sent
    gather (good, bad) (x, y)
        | x == y = (good + 1, bad)
        | otherwise = (good, bad + 1)

type DataSet = [(Xs, Ys)]

goodAndBad' :: Model -> DataSet -> (Int, Int)
goodAndBad' crf dataset =
    let add (g, b) (g', b') = (g + g', b + b')
    in  foldl add (0, 0) [goodAndBad crf x y | (x, y) <- dataset]

-- | Parallel accuracy computation.
accuracy :: Model -> DataSet -> Double
accuracy crf dataset =
    let k = numCapabilities
    	parts = partition k dataset
        xs = parMap rseq (goodAndBad' crf) parts
        (good, bad) = foldl add (0, 0) xs
        add (g, b) (g', b') = (g + g', b + b')
    in  fromIntegral good / fromIntegral (good + bad)

--------------------------------------------------------------

-- prob :: L.Vect t Int => Model -> Sent Int t -> Double
-- prob crf sent =
--     sum [ phiOn crf sent k
--         | k <- [0 .. (length sent) - 1] ]
--     - zx' crf sent
-- 
-- -- TODO: Wziac pod uwage "Regularization Variance" !
-- cll :: Model -> [Sentence] -> Double
-- cll crf dataset = sum [prob crf sent | sent <- dataset]

-- prob2 :: SentR s => Model -> ProbArray -> ProbArray -> s
--       -> Int -> Lb -> Lb -> Double
-- prob2 crf alpha beta sent k x y
--     = alpha (k - 1) y + beta (k + 1) x
--     + phi crf (observationsOn sent k) a b
--     - zxBeta beta
--   where
--     a = interp sent k       x
--     b = interp sent (k - 1) y

prob2 :: Model -> ProbArray -> ProbArray -> Int -> (Lb -> L.LogFloat)
      -> Lb -> Lb -> FeatIx -> L.LogFloat
prob2 crf alpha beta k psi x y ix
    = alpha (k - 1) y * beta (k + 1) x
    * psi x * valueL crf ix / zxBeta beta

-- prob1 :: SentR s => Model -> ProbArray -> ProbArray
--       -> s -> Int -> Label -> Double
-- prob1 crf alpha beta sent k x = logSum
--     [ prob2 crf alpha beta sent k x y
--     | y <- interpIxs sent (k - 1) ]

prob1 :: ProbArray -> ProbArray -> Int -> Lb -> L.LogFloat
prob1 alpha beta k x =
    alpha k x * beta (k + 1) x / zxBeta beta

expectedFeaturesOn
    :: Model -> ProbArray -> ProbArray -> Xs
    -> Int -> [(FeatIx, L.LogFloat)]
expectedFeaturesOn crf alpha beta sent k =
    tFeats ++ oFeats
  where
    psi = computePsi crf sent k
    pr1 = prob1     alpha beta k
    pr2 = prob2 crf alpha beta k psi

    oFeats = [ (ix, pr1 x) 
             | o <- unX (sent V.! k)
             , (x, ix) <- obIxs crf o ]

    tFeats
        | k == 0 = 
            [ (ix, pr1 x) 
            | (x, ix) <- sgIxs crf ]
        | otherwise =
            [ (ix, pr2 x y ix) 
            | x <- lbSet crf
            , (y, ix) <- prevIxs crf x ]

expectedFeaturesIn :: Model -> Xs -> [(FeatIx, L.LogFloat)]
expectedFeaturesIn crf sent = zxF `par` zxB `pseq` zxF `pseq`
    concat [expectedOn k | k <- [0 .. V.length sent - 1] ]
  where
    expectedOn = expectedFeaturesOn crf alpha beta sent
    alpha = forward sum crf sent
    beta = backward sum crf sent
    zxF = zxAlpha sent alpha
    zxB = zxBeta beta
