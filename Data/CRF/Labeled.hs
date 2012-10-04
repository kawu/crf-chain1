{-# LANGUAGE RecordWildCards #-}

module Data.CRF.Labeled
( Labeled (..)
, normalize
, rmDups
) where

import qualified Data.Map as M

-- | Labeled word: set of observations + chosen labels with
-- assigned probabilities.
-- TODO: probabilities in log scale or not?! Rather not, check
-- if they are "logged" afterwards...
data Labeled a b = Labeled
    { obs       :: [a]
    , choice    :: [(b, Double)] }
    deriving (Show, Read, Eq, Ord)

-- | Word probability normalization.
normalize :: Labeled a b -> Labeled a b
normalize word =
    word { choice = doIt (choice word) }
  where
    doIt xs =
        let z = sum (map snd xs)
        in  [(t, w / z) | (t, w) <- xs]

instance Functor (Labeled a) where
    fmap f Labeled{..} = Labeled
        { obs = obs
        , choice = [(f x, pr) | (x, pr) <- choice] }

-- | Remove duplicate labels.
rmDups :: Ord b => Labeled a b -> Labeled a b
rmDups Labeled{..} = Labeled
    { obs = obs
    , choice = nubM choice }
  where
    nubM = M.toList . M.fromListWith (+)
