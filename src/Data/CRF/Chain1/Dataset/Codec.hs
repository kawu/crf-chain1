module Data.CRF.Chain1.Dataset.Codec
( Codec
, CodecM

, encodeWord'Cu
, encodeWord'Cn
, encodeSent'Cu
, encodeSent'Cn
, encodeSent

, encodeWordL'Cu
, encodeWordL'Cn
, encodeSentL'Cu
, encodeSentL'Cn
, encodeSentL

, decodeLabel
, decodeLabels

, mkCodec
, encodeData
, encodeDataL
) where

import Prelude hiding (Word)

import Control.Applicative ((<$>), (<*>), pure)
import Data.Maybe (catMaybes)
-- import Data.Lens.Common (fstLens, sndLens)
import Data.Lens.Light (Lens, lens)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Control.Monad.Codec as C

import Data.CRF.Chain1.Dataset.Internal
import Data.CRF.Chain1.Dataset.External

-- | A codec.  The first component is used to encode observations
-- of type a, the second one is used to encode labels of type b.
type Codec a b = (C.AtomCodec a, C.AtomCodec b)

-- | Type synonym for the codec monad.  It is important to notice that by a
-- codec we denote here a structure of two 'C.AtomCodec's while in the
-- monad-codec package it denotes a monad.
type CodecM a b c = C.Codec (Codec a b) c

-- | Encode the labeled word and update the codec.
encodeWordL'Cu :: (Ord a, Ord b) => WordL a b -> CodecM a b (X, Y)
encodeWordL'Cu word = do
    x <- mkX . map Ob <$>
        mapM (C.encode' fstLens) (S.toList $ fst word)
    y <- mkY <$> sequence
    	[ (,) <$> (Lb <$> C.encode sndLens lb) <*> pure pr
	| (lb, pr) <- (M.toList . unDist) (snd word) ]
    return (x, y)

-- | Encodec the labeled word and do *not* update the codec.
-- If the label is not in the codec, use the default value.
encodeWordL'Cn :: (Ord a, Ord b) => Int -> WordL a b -> CodecM a b (X, Y)
encodeWordL'Cn i word = do
    x <- mkX . map Ob . catMaybes <$>
        mapM (C.maybeEncode fstLens) (S.toList $ fst word)
    y <- mkY <$> sequence
    	[ (,) <$> encodeL i lb <*> pure pr
	| (lb, pr) <- (M.toList . unDist) (snd word) ]
    return (x, y)
  where
    encodeL j y = Lb . maybe j id <$> C.maybeEncode sndLens y

-- | Encode the word and update the codec.
encodeWord'Cu :: Ord a => Word a -> CodecM a b X
encodeWord'Cu word =
    mkX . map Ob <$> mapM (C.encode' fstLens) (S.toList word)

-- | Encode the word and do *not* update the codec.
encodeWord'Cn :: Ord a => Word a -> CodecM a b X
encodeWord'Cn word = 
    mkX . map Ob . catMaybes <$> mapM (C.maybeEncode fstLens) (S.toList word)

-- | Encode the labeled sentence and update the codec.
encodeSentL'Cu :: (Ord a, Ord b) => SentL a b -> CodecM a b (Xs, Ys)
encodeSentL'Cu sent = do
    ps <- mapM encodeWordL'Cu sent
    return (V.fromList (map fst ps), V.fromList (map snd ps))

-- | Encode the labeled sentence and do *not* update the codec.
-- Substitute the default label for any label not present in the codec.
encodeSentL'Cn :: (Ord a, Ord b) => b -> SentL a b -> CodecM a b (Xs, Ys)
encodeSentL'Cn def sent = do
    i <- C.maybeEncode sndLens def >>= \mi -> case mi of
        Just _i -> return _i
        Nothing -> error "encodeWordL'Cn: default label not in the codec"
    ps <- mapM (encodeWordL'Cn i) sent
    return (V.fromList (map fst ps), V.fromList (map snd ps))

-- | Encode the labeled sentence with the given codec.  Substitute the
-- default label for any label not present in the codec.
encodeSentL :: (Ord a, Ord b) => b -> Codec a b -> SentL a b -> (Xs, Ys)
encodeSentL def codec = C.evalCodec codec . encodeSentL'Cn def

-- | Encode the sentence and update the codec.
encodeSent'Cu :: Ord a => Sent a -> CodecM a b Xs
encodeSent'Cu = fmap V.fromList . mapM encodeWord'Cu

-- | Encode the sentence and do *not* update the codec.
encodeSent'Cn :: Ord a => Sent a -> CodecM a b Xs
encodeSent'Cn = fmap V.fromList . mapM encodeWord'Cn

-- | Encode the sentence using the given codec.
encodeSent :: Ord a => Codec a b -> Sent a -> Xs
encodeSent codec = C.evalCodec codec . encodeSent'Cn

-- | Create the codec on the basis of the labeled dataset, return the
-- resultant codec and the encoded dataset.
mkCodec :: (Ord a, Ord b) => [SentL a b] -> (Codec a b, [(Xs, Ys)])
mkCodec =
    let swap (x, y) = (y, x)
    in  swap . C.runCodec (C.empty, C.empty) . mapM encodeSentL'Cu

-- | Encode the labeled dataset using the codec.  Substitute the default
-- label for any label not present in the codec.
encodeDataL :: (Ord a, Ord b) => b -> Codec a b -> [SentL a b] -> [(Xs, Ys)]
encodeDataL def codec = C.evalCodec codec . mapM (encodeSentL'Cn def)

-- | Encode the dataset with the codec.
encodeData :: Ord a => Codec a b -> [Sent a] -> [Xs]
encodeData codec = map (encodeSent codec)

-- | Decode the label.
decodeLabel :: Ord b => Codec a b -> Lb -> b
decodeLabel codec x = C.evalCodec codec $ C.decode sndLens (unLb x)

-- | Decode the sequence of labels.
decodeLabels :: Ord b => Codec a b -> [Lb] -> [b]
decodeLabels codec xs = C.evalCodec codec $
    sequence [C.decode sndLens (unLb x) | x <- xs]


-- * Stock lenses

fstLens :: Lens (a,b) a
-- fstLens = Lens $ \(a,b) -> store (\ a' -> (a', b)) a
fstLens = lens fst (\a' (a, b) -> (a', b))

sndLens :: Lens (a,b) b
-- sndLens = Lens $ \(a,b) -> store (\ b' -> (a, b')) b
sndLens = lens snd (\b' (a, b) -> (a, b'))
