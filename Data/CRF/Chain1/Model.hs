{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Internal implementation of the CRF model.

module Data.CRF.Chain1.Model
( FeatIx (..)
, Model (..)
, mkModel
, lbSet
, valueL
, featToIx
, featToInt
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
import qualified Data.Vector.Generic.Base as G
import qualified Data.Vector.Generic.Mutable as G
import qualified Data.Set as Set
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import qualified Data.Number.LogFloat as L

import Data.CRF.Chain1.Dataset.Internal
import Data.CRF.Chain1.Feature

-- | A feature index.  To every model feature a unique index is assigned.
newtype FeatIx = FeatIx { unFeatIx :: Int }
    deriving ( Show, Eq, Ord, Binary
             , G.Vector U.Vector, G.MVector U.MVector, U.Unbox )

-- | A label and a feature index determined by that label.
type LbIx   = (Lb, FeatIx)

dummyFeatIx :: FeatIx
dummyFeatIx = FeatIx (-1)

isDummy :: FeatIx -> Bool
isDummy (FeatIx ix) = ix < 0

notDummy :: FeatIx -> Bool
notDummy = not . isDummy

-- | The model is realy a map from features to potentials, but for the sake
-- of efficiency the internal representation is more complex.
data Model = Model {
    -- | Value (potential) of the model for feature index.
      values    :: U.Vector Double
    -- | A map from features to feature indices
    , ixMap     :: M.Map Feature FeatIx
    -- | Number of labels. The label set is of the {0, 1, .., lbNum - 1}
    -- form, which is guaranteed by the codec.
    , lbNum 	:: Int
    -- | Singular feature index for the given label.  Index is equall to -1
    -- if feature is not present in the model.
    , sgIxsV 	:: U.Vector FeatIx
    -- | Set of labels for the given observation which, together with the
    -- observation, constitute an observation feature of the model. 
    , obIxsV    :: V.Vector (U.Vector LbIx)
    -- | Set of ,,previous'' labels for the value of the ,,current'' label.
    -- Both labels constitute a transition feature present in the the model.
    , prevIxsV  :: V.Vector (U.Vector LbIx)
    -- | Set of ,,next'' labels for the value of the ,,current'' label.
    -- Both labels constitute a transition feature present in the the model.
    , nextIxsV  :: V.Vector (U.Vector LbIx) }

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
    
        _obSet  = nub $ concatMap (featObs . fst) fs
        _obNum = length _obSet
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

        _obIxsV = adjVects _obNum
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
      . checkSet (map unOb _obSet)
      $ crf

-- | Construct the model from the list of features.  All parameters will be
-- set to 0.  There may be repetitions in the input list.
mkModel :: [Feature] -> Model
mkModel fs =
    let fSet = Set.fromList fs
        fs'  = Set.toList fSet
        vs   = replicate (Set.size fSet) 0.0
    in  fromList (zip fs' vs)

-- | List of labels [0 .. 'lbNum' - 1].
lbSet :: Model -> [Lb]
lbSet crf = map Lb [0 .. lbNum crf - 1]

-- | Model potential defined for the given feature interpreted as a
-- number in logarithmic domain.
valueL :: Model -> FeatIx -> L.LogFloat
valueL crf (FeatIx i) = L.logToLogFloat (values crf U.! i)
{-# INLINE valueL #-}

-- | Determine the index for the given feature.
featToIx :: Model -> Feature -> FeatIx
featToIx crf feat = ixMap crf M.! feat
{-# INLINE featToIx #-}

-- | Same as 'featToIx' but immediately unwrap the feature index to
-- integer value.
featToInt :: Model -> Feature -> Int
featToInt crf = unFeatIx . featToIx crf
{-# INLINE featToInt #-}

-- | Potential value (in log domain) of the singular feature with the
-- given label.  The value defaults to 0 when the feature is not a
-- member of the model.
sgValue :: Model -> Lb -> L.LogFloat
sgValue crf (Lb x) = 
    case unFeatIx (sgIxsV crf U.! x) of
        -- FIXME: shoule not be -Infinity? It looks like it doesn't
        -- matter much for know, because that value is ignored anyway,
        -- but the SFeature which is not a member of the model
        -- should have probability of 0.
        -1 -> 0		
        ix -> L.logToLogFloat (values crf U.! ix)

-- | List of labels which can be located on the first position of
-- a sentence together with feature indices determined by them.
sgIxs :: Model -> [LbIx]
sgIxs crf
    = filter (notDummy . snd)
    . zip (map Lb [0..])
    . U.toList $ sgIxsV crf
{-# INLINE sgIxs #-}

-- | List of labels which constitute a valid feature in combination with
-- the given observation accompanied by feature indices determined by
-- these labels.
obIxs :: Model -> Ob -> [LbIx]
obIxs crf x = U.toList (obIxsV crf V.! unOb x)
{-# INLINE obIxs #-}

-- | List of ,,next'' labels which constitute a valid feature in combination
-- with the ,,current'' label accompanied by feature indices determined by
-- ,,next'' labels.
nextIxs :: Model -> Lb -> [LbIx]
nextIxs crf x = U.toList (nextIxsV crf V.! unLb x)
{-# INLINE nextIxs #-}

-- | List of ,,previous'' labels which constitute a valid feature in
-- combination with the ,,current'' label accompanied by feature indices
-- determined by ,,previous'' labels.
prevIxs :: Model -> Lb -> [LbIx]
prevIxs crf x = U.toList (prevIxsV crf V.! unLb x)
{-# INLINE prevIxs #-}

nub :: Ord a => [a] -> [a] 
nub = Set.toList . Set.fromList
