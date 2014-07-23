module Data.CRF.Chain1.Dataset.Codec
( Codec
, CodecM
, obMax
, lbMax

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


import Control.Applicative ((<$>), (<*>), pure)
import Data.Maybe (catMaybes)
import Data.Lens.Common (fstLens, sndLens)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Control.Monad.Codec as C

import Data.CRF.Chain1.Dataset.Internal
import Data.CRF.Chain1.Dataset.External


-- | A codec.  The first component is used to encode observations
-- of type a, the second one is used to encode labels of type b.
type Codec a b = (C.AtomCodec a, C.AtomCodec b)

-- | The maximum internal observation included in the codec.
obMax :: Codec a b -> Ob
obMax =
    let idMax m = M.size m - 1
    in  Ob . idMax . C.to . fst

-- | The maximum internal label included in the codec.
lbMax :: Codec a b -> Lb
lbMax =
    let idMax m = M.size m - 1
    in  Lb . idMax . C.to . snd

-- | The empty codec.
empty :: Ord b => Codec a b
empty = (C.empty, C.empty)

-- | Type synonym for the codec monad.  It is important to notice that by a
-- codec we denote here a structure of two 'C.AtomCodec's while in the
-- monad-codec package it denotes a monad.
type CodecM a b c = C.Codec (Codec a b) c

-- | Encode the observation and update the codec (only in the encoding
-- direction).
encodeObU :: Ord a => a -> CodecM a b Ob
encodeObU = fmap Ob . C.encode' fstLens

-- | Encode the observation and do *not* update the codec.
encodeObN :: Ord a => a -> CodecM a b (Maybe Ob)
encodeObN = fmap (fmap Ob) . C.maybeEncode fstLens

-- | Encode the label and update the codec.
encodeLbU :: Ord b => b -> CodecM a b Lb
encodeLbU = fmap Lb . C.encode sndLens

-- | Encode the label and do *not* update the codec.
-- If the label is not in the codec, use the default value.
encodeLbN :: Ord b => b -> b -> CodecM a b Lb
encodeLbN def x = do
    i <- C.maybeEncode sndLens def >>= \mi -> case mi of
        Just _i -> return _i
        Nothing -> error "encodeWordL'Cn: default label not in the codec"
    my <- C.maybeEncode sndLens x -- (Just x)
    Lb <$> ( case my of
        Just y  -> return y
        Nothing -> return i )
        -- Nothing -> fromJust <$> C.maybeEncode sndLens Nothing )

-- | Encode the word and update the codec.
encodeWord'Cu :: (Ord a) => Word a -> CodecM a b X
encodeWord'Cu word = mkX <$> mapM encodeObU (S.toList word)

-- | Encode the labeled word and update the codec.
encodeWordL'Cu :: (Ord a, Ord b) => WordL a b -> CodecM a b (X, Y)
encodeWordL'Cu word = do
    x <- encodeWord'Cu $ fst word
    y <- mkY <$> sequence
    	[ (,) <$> encodeLbU lb <*> pure pr
	| (lb, pr) <- (M.toList . unDist) (snd word) ]
    return (x, y)

-- | Encode the word and do *not* update the codec.
encodeWord'Cn :: (Ord a) => Word a -> CodecM a b X
encodeWord'Cn word = mkX . catMaybes <$> mapM encodeObN (S.toList word)

-- | Encodec the labeled word and do *not* update the codec.
encodeWordL'Cn :: (Ord a, Ord b) => b -> WordL a b -> CodecM a b (X, Y)
encodeWordL'Cn def word = do
    x <- encodeWord'Cn $ fst word
    y <- mkY <$> sequence
    	[ (,) <$> encodeLbN def lb <*> pure pr
	| (lb, pr) <- (M.toList . unDist) (snd word) ]
    return (x, y)

-- | Encode the labeled sentence and update the codec.
encodeSentL'Cu :: (Ord a, Ord b) => SentL a b -> CodecM a b (Xs, Ys)
encodeSentL'Cu sent = do
    ps <- mapM (encodeWordL'Cu) sent
    return (V.fromList (map fst ps), V.fromList (map snd ps))

-- | Encode the labeled sentence and do *not* update the codec.
-- Substitute the default label for any label not present in the codec.
encodeSentL'Cn :: (Ord a, Ord b) => b -> SentL a b -> CodecM a b (Xs, Ys)
encodeSentL'Cn def sent = do
    ps <- mapM (encodeWordL'Cn def) sent
    return (V.fromList (map fst ps), V.fromList (map snd ps))

-- -- | Encode labels into an ascending vector of distinct label codes.
-- encodeLabels :: Ord b => Codec a b -> [b] -> AVec Lb
-- encodeLabels codec = fromList . C.evalCodec codec . mapM encodeLbN

-- | Encode the labeled sentence with the given codec.  Substitute the
-- default label for any label not present in the codec.
encodeSentL :: (Ord a, Ord b) => b -> Codec a b -> SentL a b -> (Xs, Ys)
encodeSentL def codec = C.evalCodec codec . encodeSentL'Cn def

-- | Encode the sentence and update the codec.
encodeSent'Cu :: (Ord a) => Sent a -> CodecM a b Xs
encodeSent'Cu = fmap V.fromList . mapM encodeWord'Cu

-- | Encode the sentence and do *not* update the codec.
encodeSent'Cn :: (Ord a) => Sent a -> CodecM a b Xs
encodeSent'Cn = fmap V.fromList . mapM encodeWord'Cn

-- | Encode the sentence using the given codec.
encodeSent :: (Ord a) => Codec a b -> Sent a -> Xs
encodeSent codec = C.evalCodec codec . encodeSent'Cn

-- | Create codec on the basis of the labeled dataset.
mkCodec :: (Ord a, Ord b) => [SentL a b] -> Codec a b
mkCodec = C.execCodec empty . mapM_ encodeSentL'Cu

-- | Encode the labeled dataset using the codec.  Substitute the default
-- label for any label not present in the codec.
encodeDataL :: (Ord a, Ord b) => b -> Codec a b -> [SentL a b] -> [(Xs, Ys)]
encodeDataL def = map . encodeSentL def

-- | Encode the dataset with the codec.
encodeData :: (Ord a) => Codec a b -> [Sent a] -> [Xs]
encodeData = map . encodeSent

-- | Decode the label.
decodeLabel :: Ord b => Codec a b -> Lb -> b
decodeLabel codec x = C.evalCodec codec $ C.decode sndLens (unLb x)

-- | Decode the sequence of labels.
decodeLabels :: Ord b => Codec a b -> [Lb] -> [b]
decodeLabels codec xs = C.evalCodec codec $
    sequence [C.decode sndLens (unLb x) | x <- xs]

-- hasLabel :: Ord b => Codec a b -> b -> Bool
-- hasLabel codec x = M.member (Just x) (C.to $ snd codec)
-- {-# INLINE hasLabel #-}
-- 
-- -- | Return the label when 'Just' or one of the unknown values
-- -- when 'Nothing'.
-- unJust :: Ord b => Codec a b -> Word a b -> Maybe b -> b
-- unJust _ _ (Just x) = x
-- unJust codec word Nothing = case allUnk of
--     (x:_)   -> x
--     []      -> error "unJust: Nothing and all values known"
--   where
--     allUnk = filter (not . hasLabel codec) (S.toList $ lbs word)
-- 
-- -- | Replace 'Nothing' labels with all unknown labels from
-- -- the set of potential interpretations.
-- unJusts :: Ord b => Codec a b -> Word a b -> [Maybe b] -> [b]
-- unJusts codec word xs =
--     concatMap deJust xs
--   where
--     allUnk = filter (not . hasLabel codec) (S.toList $ lbs word)
--     deJust (Just x) = [x]
--     deJust Nothing  = allUnk







--------------------------------------
-- BACKUP
--------------------------------------
-- 
-- 
-- -- | A codec.  The first component is used to encode observations
-- -- of type a, the second one is used to encode labels of type b.
-- type Codec a b = (C.AtomCodec a, C.AtomCodec b)
-- 
-- 
-- -- | Type synonym for the codec monad.  It is important to notice that by a
-- -- codec we denote here a structure of two 'C.AtomCodec's while in the
-- -- monad-codec package it denotes a monad.
-- type CodecM a b c = C.Codec (Codec a b) c
-- 
-- 
-- -- | The maximum internal observation included in the codec.
-- obMax :: Codec a b -> Ob
-- obMax =
--     let idMax m = M.size m - 1
--     in  Ob . idMax . C.to . fst
-- 
-- 
-- -- | The maximum internal labels included in the codec.
-- lbMax :: Codec a b -> Lb
-- lbMax =
--     let idMax m = M.size m - 1
--     in  Lb . idMax . C.to . snd
-- 
-- 
-- -- | Encode the labeled word and update the codec.
-- encodeWordL'Cu :: (Ord a, Ord b) => WordL a b -> CodecM a b (X, Y)
-- encodeWordL'Cu word = do
--     x <- mkX . map Ob <$>
--         mapM (C.encode' fstLens) (S.toList $ fst word)
--     y <- mkY <$> sequence
--     	[ (,) <$> (Lb <$> C.encode sndLens lb) <*> pure pr
-- 	| (lb, pr) <- (M.toList . unDist) (snd word) ]
--     return (x, y)
-- 
-- -- | Encodec the labeled word and do *not* update the codec.
-- -- If the label is not in the codec, use the default value.
-- encodeWordL'Cn :: (Ord a, Ord b) => Int -> WordL a b -> CodecM a b (X, Y)
-- encodeWordL'Cn i word = do
--     x <- mkX . map Ob . catMaybes <$>
--         mapM (C.maybeEncode fstLens) (S.toList $ fst word)
--     y <- mkY <$> sequence
--     	[ (,) <$> encodeL i lb <*> pure pr
-- 	| (lb, pr) <- (M.toList . unDist) (snd word) ]
--     return (x, y)
--   where
--     encodeL j y = Lb . maybe j id <$> C.maybeEncode sndLens y
-- 
-- -- | Encode the word and update the codec.
-- encodeWord'Cu :: Ord a => Word a -> CodecM a b X
-- encodeWord'Cu word =
--     mkX . map Ob <$> mapM (C.encode' fstLens) (S.toList word)
-- 
-- -- | Encode the word and do *not* update the codec.
-- encodeWord'Cn :: Ord a => Word a -> CodecM a b X
-- encodeWord'Cn word = 
--     mkX . map Ob . catMaybes <$> mapM (C.maybeEncode fstLens) (S.toList word)
-- 
-- -- | Encode the labeled sentence and update the codec.
-- encodeSentL'Cu :: (Ord a, Ord b) => SentL a b -> CodecM a b (Xs, Ys)
-- encodeSentL'Cu sent = do
--     ps <- mapM encodeWordL'Cu sent
--     return (V.fromList (map fst ps), V.fromList (map snd ps))
-- 
-- -- | Encode the labeled sentence and do *not* update the codec.
-- -- Substitute the default label for any label not present in the codec.
-- encodeSentL'Cn :: (Ord a, Ord b) => b -> SentL a b -> CodecM a b (Xs, Ys)
-- encodeSentL'Cn def sent = do
--     i <- C.maybeEncode sndLens def >>= \mi -> case mi of
--         Just _i -> return _i
--         Nothing -> error "encodeWordL'Cn: default label not in the codec"
--     ps <- mapM (encodeWordL'Cn i) sent
--     return (V.fromList (map fst ps), V.fromList (map snd ps))
-- 
-- -- | Encode the labeled sentence with the given codec.  Substitute the
-- -- default label for any label not present in the codec.
-- encodeSentL :: (Ord a, Ord b) => b -> Codec a b -> SentL a b -> (Xs, Ys)
-- encodeSentL def codec = C.evalCodec codec . encodeSentL'Cn def
-- 
-- -- | Encode the sentence and update the codec.
-- encodeSent'Cu :: Ord a => Sent a -> CodecM a b Xs
-- encodeSent'Cu = fmap V.fromList . mapM encodeWord'Cu
-- 
-- -- | Encode the sentence and do *not* update the codec.
-- encodeSent'Cn :: Ord a => Sent a -> CodecM a b Xs
-- encodeSent'Cn = fmap V.fromList . mapM encodeWord'Cn
-- 
-- -- | Encode the sentence using the given codec.
-- encodeSent :: Ord a => Codec a b -> Sent a -> Xs
-- encodeSent codec = C.evalCodec codec . encodeSent'Cn
-- 
-- -- | Create the codec on the basis of the labeled dataset, return the
-- -- resultant codec and the encoded dataset.
-- mkCodec :: (Ord a, Ord b) => [SentL a b] -> (Codec a b, [(Xs, Ys)])
-- mkCodec =
--     let swap (x, y) = (y, x)
--     in  swap . C.runCodec (C.empty, C.empty) . mapM encodeSentL'Cu
-- 
-- -- | Encode the labeled dataset using the codec.  Substitute the default
-- -- label for any label not present in the codec.
-- encodeDataL :: (Ord a, Ord b) => b -> Codec a b -> [SentL a b] -> [(Xs, Ys)]
-- encodeDataL def codec = C.evalCodec codec . mapM (encodeSentL'Cn def)
-- 
-- -- | Encode the dataset with the codec.
-- encodeData :: Ord a => Codec a b -> [Sent a] -> [Xs]
-- encodeData codec = map (encodeSent codec)
-- 
-- -- | Decode the label.
-- decodeLabel :: Ord b => Codec a b -> Lb -> b
-- decodeLabel codec x = C.evalCodec codec $ C.decode sndLens (unLb x)
-- 
-- -- | Decode the sequence of labels.
-- decodeLabels :: Ord b => Codec a b -> [Lb] -> [b]
-- decodeLabels codec xs = C.evalCodec codec $
--     sequence [C.decode sndLens (unLb x) | x <- xs]
