module Data.CRF.Codec
( Codec
, encodeWord
, encodeSent
, encodeWord'
, encodeSent'
) where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Maybe (catMaybes)
import qualified Data.Vector as V
import qualified Control.Monad.Codec as C
import Control.Lens (_1, _2)

import Data.CRF.Core
import Data.CRF.Labeled

type Codec a b = (C.AtomCodec a, C.AtomCodec b)

encodeWord :: Ord a => Codec a b -> [a] -> X
encodeWord codec word = C.evalCodec codec $ do
    mkX . map Ob . catMaybes <$> mapM (C.maybeEncode _1) word

encodeWord' :: (Ord a, Ord b) => Codec a b -> Labeled a b -> (X, Y)
encodeWord' codec word = C.evalCodec codec $ do
    x <- mkX . map Ob . catMaybes <$> mapM (C.maybeEncode _1) (obs word)
    y <- mkY <$> sequence
    	[ (,) <$> encodeL lb <*> pure pr
	| (lb, pr) <- choice word ]
    return (x, y)
  where
    encodeL y = C.maybeEncode _2 y >>= \mi -> case mi of
        Nothing -> error "encodeWord': unknown label"
        Just i  -> return (Lb i)

encodeSent :: Ord a => Codec a b -> [[a]] -> Xs
encodeSent codec = V.fromList . map (encodeWord codec)

encodeSent' :: (Ord a, Ord b) => Codec a b -> [Labeled a b] -> (Xs, Ys)
encodeSent' codec ws =
    (V.fromList xs, V.fromList ys)
  where
    ps = map (encodeWord' codec) ws
    xs = map fst ps
    ys = map snd ps
