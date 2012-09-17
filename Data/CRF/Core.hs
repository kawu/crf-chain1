{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.CRF.Core
( Ob (..)
, Lb (..)
, FeatIx (..)

, Sent
, Dataset

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
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Array as A

-- | An observation.
newtype Ob = Ob { unOb :: Int }
    deriving ( Show, Read, Eq, Ord, Binary
             , Vector U.Vector, MVector U.MVector, U.Unbox )

-- | A label.
newtype Lb = Lb { unLb :: Int }
    deriving ( Show, Read, Eq, Ord, Binary
             , Vector U.Vector, MVector U.MVector, U.Unbox )

-- | A feature index.  To every model feature a unique index is assigned.
newtype FeatIx = FeatIx { unFeatIx :: Int }
    deriving ( Show, Read, Eq, Ord, Binary
             , Vector U.Vector, MVector U.MVector, U.Unbox )

-- | A setence.
type Sent a = V.Vector a

-- | A dataset.
type Dataset a b = V.Vector (Sent a, Sent b)

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
type Xs = Sent X

-- | Simple word represented by a list of its observations.
-- It is related to Data.CRF.R type by R's invariant.
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
type Ys = Sent Y
