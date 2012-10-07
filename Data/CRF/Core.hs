{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.CRF.Core
( Ob (..)
, Lb (..)
, FeatIx (..)

, X (..)
, mkX
, unX
, Xs

, Y (..)
, mkY
, unY
, Ys
) where

import Data.Vector.Generic.Base
import Data.Vector.Generic.Mutable
import Data.Binary (Binary)
import Data.Ix (Ix)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

-- | An observation.
newtype Ob = Ob { unOb :: Int }
    deriving ( Show, Read, Eq, Ord, Binary
             , Vector U.Vector, MVector U.MVector, U.Unbox )

-- | A label.
newtype Lb = Lb { unLb :: Int }
    deriving ( Show, Read, Eq, Ord, Binary
             , Vector U.Vector, MVector U.MVector, U.Unbox
	     , Num, Ix )

-- | A feature index.  To every model feature a unique index is assigned.
newtype FeatIx = FeatIx { unFeatIx :: Int }
    deriving ( Show, Read, Eq, Ord, Binary
             , Vector U.Vector, MVector U.MVector, U.Unbox )

-- | Simple word represented by a list of its observations.
newtype X = X { _unX :: U.Vector Ob }
    deriving (Show, Read, Eq, Ord)

-- | X constructur.
mkX :: [Ob] -> X
mkX = X . U.fromList
{-# INLINE mkX #-}

-- | X deconstructur symetric to mkX.
unX :: X -> [Ob]
unX = U.toList . _unX
{-# INLINE unX #-}

-- | Sentence of words.
type Xs = V.Vector X

-- | Probability distribution over labels. 
newtype Y = Y { _unY :: U.Vector (Lb, Double) }
    deriving (Show, Read, Eq, Ord)

-- | Y constructor.
mkY :: [(Lb, Double)] -> Y
mkY = Y . U.fromList
{-# INLINE mkY #-}

-- | Y deconstructor symetric to mkY.
unY :: Y -> [(Lb, Double)]
unY = U.toList . _unY
{-# INLINE unY #-}

-- | Sentence of Y (label choices).
type Ys = V.Vector Y
