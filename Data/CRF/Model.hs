module Data.CRF.Model
( Model (..)
, mkModel
, lbSet
, valueL
, featToIx
, sgValue
, sgIxs
, obIxs
, nextIxs
, prevIxs
) where

import Control.Applicative ((<$>), (<*>))
import Data.List (groupBy, sort)
import Data.Function (on)
import Data.Binary
import Data.Vector.Binary ()
import qualified Data.Set as Set
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import qualified Data.Number.LogFloat as L

import Data.CRF.Core
import Data.CRF.Feature
-- import           Data.CRF.LogMath (mInf)

type LbIx   = (Lb, FeatIx)

dummyFeatIx :: FeatIx
dummyFeatIx = FeatIx (-1)

isDummy :: FeatIx -> Bool
isDummy (FeatIx ix) = ix < 0

notDummy :: FeatIx -> Bool
notDummy = not . isDummy

data Model = Model
    -- | Model values.
    { values    :: U.Vector Double
    -- | Indices map.
    , ixMap     :: M.Map Feature FeatIx
    -- | Number of labels.
    , lbNum 	:: Int
    -- | Singular feature indices.  Index is equall to -1 if feature
    -- is not present in the model.
    , sgIxsV 	:: U.Vector FeatIx
    -- | Set of acceptable labels when known value of the observation.
    , obIxsV    :: V.Vector (U.Vector LbIx)
    -- | Set of "previous" labels when known value of the current label.
    , prevIxsV  :: V.Vector (U.Vector LbIx)
    -- | Set of "next" labels when known value of the current label.
    , nextIxsV  :: V.Vector (U.Vector LbIx) }

-- instance ParamCore Model where
-- 
--     unsafeConsume f xs crf = do
--         values' <- unsafeConsume f xs $ values crf
--         return $ crf { values = values' }
-- 
--     unsafeMap f crf = do
--         values' <- unsafeMap f $ values crf
--         return $ crf { values = values' }
-- 
--     size = size . values

instance Binary Model where
    put crf = do
        put $ values crf
        put $ ixMap crf
        put $ lbNum crf
        put $ sgIxsV crf
        put $ obIxsV crf
        put $ prevIxsV crf
        put $ nextIxsV crf
    get = Model <$> get <*> get <*> get <*> get <*> get <*> get <*> get

-- | Construct CRF model from associations list.  There should be
-- no repetition of features in the input list.
fromList :: [(Feature, Double)] -> Model
fromList fs =
    let featLbs (SFeature x) = [x]
    	featLbs (OFeature _ x) = [x]
        featLbs (TFeature x y) = [x, y]
        featObs (OFeature o _) = [o]
        featObs _ = []

        _ixMap = M.fromList $ zip
            (map fst fs)
            (map FeatIx [0..])
    
        obSet  = nub $ concatMap (featObs . fst) fs
        _lbSet = nub $ concatMap (featLbs . fst) fs
        _lbNum = length _lbSet

        sFeats = [feat | (feat, _val) <- fs, isSFeat feat]
        tFeats = [feat | (feat, _val) <- fs, isTFeat feat]
        oFeats = [feat | (feat, _val) <- fs, isOFeat feat]
        
        _sgIxsV = sgVects _lbNum
            [ (unLb x, featToIx crf feat)
            | feat@(SFeature x) <- sFeats ]

        _prevIxsV = adjVects _lbNum
            [ (unLb x, (y, featToIx crf feat))
            | feat@(TFeature x y) <- tFeats ]

        _nextIxsV = adjVects _lbNum
            [ (unLb y, (x, featToIx crf feat))
            | feat@(TFeature x y) <- tFeats ]

        _obIxsV = adjVects _lbNum
            [ (unOb o, (x, featToIx crf feat))
            | feat@(OFeature o x) <- oFeats ]

        -- | Adjacency vectors.
        adjVects n xs =
            V.replicate n (U.fromList []) V.// update
          where
            update = map mkVect $ groupBy ((==) `on` fst) $ sort xs
            mkVect (y:ys) = (fst y, U.fromList $ sort $ map snd (y:ys))
            mkVect [] = error "mkVect: null list"

        sgVects n xs = U.replicate n dummyFeatIx U.// xs

        _values = U.replicate (length fs) 0.0
            U.// [ (featToInt crf feat, val)
                 | (feat, val) <- fs ]

        checkSet set cont = if set == [0 .. length set - 1]
            then cont
            else error "Model.fromList: basic assumption not fulfilled"

        crf = Model _values _ixMap _lbNum _sgIxsV _obIxsV _prevIxsV _nextIxsV
    in  checkSet (map unLb _lbSet)
      . checkSet (map unOb obSet)
      $ crf

mkModel :: [Feature] -> Model
mkModel fs =
    let fSet = Set.fromList fs
        fs'  = Set.toList fSet
        vs   = replicate (Set.size fSet) 0.0
    in  fromList (zip fs' vs)

lbSet :: Model -> [Lb]
lbSet crf = map Lb [0 .. lbNum crf - 1]

valueL :: Model -> FeatIx -> L.LogFloat
valueL crf (FeatIx i) = L.logToLogFloat (values crf U.! i)
{-# INLINE valueL #-}

featToIx :: Model -> Feature -> FeatIx
featToIx crf feat = ixMap crf M.! feat
{-# INLINE featToIx #-}

featToInt :: Model -> Feature -> Int
featToInt crf = unFeatIx . featToIx crf
{-# INLINE featToInt #-}

sgValue :: Model -> Lb -> L.LogFloat
sgValue crf (Lb x) = 
    case unFeatIx (sgIxsV crf U.! x) of
        -1 -> 0
        ix -> L.logToLogFloat (values crf U.! ix)

sgIxs :: Model -> [LbIx]
sgIxs crf
    = filter (notDummy . snd)
    . zip (map Lb [0..])
    . U.toList $ sgIxsV crf
{-# INLINE sgIxs #-}

obIxs :: Model -> Ob -> [LbIx]
obIxs crf x = U.toList (obIxsV crf V.! unOb x)
{-# INLINE obIxs #-}

nextIxs :: Model -> Lb -> [LbIx]
nextIxs crf x = U.toList (nextIxsV crf V.! unLb x)
{-# INLINE nextIxs #-}

prevIxs :: Model -> Lb -> [LbIx]
prevIxs crf x = U.toList (prevIxsV crf V.! unLb x)
{-# INLINE prevIxs #-}

nub :: Ord a => [a] -> [a] 
nub = Set.toList . Set.fromList
