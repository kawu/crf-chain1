module Data.CRF.Codec
( Codec
, CodecM
, encodeLabCu
, encodeLabCn
, encodeWordCu
, encodeWordCn
, encodeLsCu
, encodeLsCn
, encodeWsCu
, encodeWsCn
, encodeLs
, encodeWs
, encodeData
) where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Maybe (catMaybes)
import qualified Data.Vector as V
import qualified Control.Monad.Codec as C
import Data.Lens.Common (fstLens, sndLens)

import Data.CRF.Core
import Data.CRF.Labeled

-- | The codec structure.
type Codec a b = (C.AtomCodec a, C.AtomCodec b)

-- | Type synonym for the codec monad.
type CodecM a b c = C.Codec (Codec a b) c

-- | Encode the labeled word and update the codec.
encodeLabCu :: (Ord a, Ord b) => Labeled a b -> CodecM a b (X, Y)
encodeLabCu word = do
    x <- mkX . map Ob <$> mapM (C.encode' fstLens) (obs word)
    y <- mkY <$> sequence
    	[ (,) <$> (Lb <$> C.encode sndLens lb) <*> pure pr
	| (lb, pr) <- choice word ]
    return (x, y)

-- | Encodec the labeled word and do *not* update the codec.
-- If the label is not in the codec, use the default value.
encodeLabCn :: (Ord a, Ord b) => Int -> Labeled a b -> CodecM a b (X, Y)
encodeLabCn i word = do
    x <- mkX . map Ob . catMaybes <$> mapM (C.maybeEncode fstLens) (obs word)
    y <- mkY <$> sequence
    	[ (,) <$> encodeL i lb <*> pure pr
	| (lb, pr) <- choice word ]
    return (x, y)
  where
    encodeL j y = Lb . maybe j id <$> C.maybeEncode sndLens y

-- | Encode the word and update the codec.
encodeWordCu :: Ord a => [a] -> CodecM a b X
encodeWordCu word =
    mkX . map Ob <$> mapM (C.encode' fstLens) word

-- | Encode the word and do *not* update the codec.
encodeWordCn :: Ord a => [a] -> CodecM a b X
encodeWordCn word = 
    mkX . map Ob . catMaybes <$> mapM (C.maybeEncode fstLens) word

-- | Encode the labeled sentence and update the codec.
encodeLsCu :: (Ord a, Ord b) => [Labeled a b] -> CodecM a b (Xs, Ys)
encodeLsCu sent = do
    ps <- mapM encodeLabCu sent
    return (V.fromList (map fst ps), V.fromList (map snd ps))

-- | Encode the labeled sentence and do *not* update the codec.
-- Substitute the default label for any label not present in the codec.
encodeLsCn :: (Ord a, Ord b) => b -> [Labeled a b] -> CodecM a b (Xs, Ys)
encodeLsCn def sent = do
    i <- C.maybeEncode sndLens def >>= \mi -> case mi of
        Just _i -> return _i
        Nothing -> error "encodeLabCn: default label not in the codec"
    ps <- mapM (encodeLabCn i) sent
    return (V.fromList (map fst ps), V.fromList (map snd ps))

-- | Encode the labeled sentence with the given codec.  Substitute the
-- default label for any label not present in the codec.
encodeLs :: (Ord a, Ord b) => b -> Codec a b -> [Labeled a b] -> (Xs, Ys)
encodeLs def codec = C.evalCodec codec . encodeLsCn def

-- | Encode the sentence and update the codec.
encodeWsCu :: Ord a => [[a]] -> CodecM a b Xs
encodeWsCu = fmap V.fromList . mapM encodeWordCu

-- | Encode the sentence and do *not* update the codec.
encodeWsCn :: Ord a => [[a]] -> CodecM a b Xs
encodeWsCn = fmap V.fromList . mapM encodeWordCn

-- | Encode the sentence using the given codec.
encodeWs :: Ord a => Codec a b -> [[a]] -> Xs
encodeWs codec = C.evalCodec codec . encodeWsCn

-- | Create the codec on the basis of the labeled dataset, return the
-- resultant codec and the encoded dataset.
encodeData :: (Ord a, Ord b) => [[Labeled a b]] -> (Codec a b, [(Xs, Ys)])
encodeData ds =
    let swap (x, y) = (y, x)
    in  swap $ C.runCodec (C.empty, C.empty) (mapM encodeLsCu ds)

-- encodeWord :: Ord a => Codec a b -> [a] -> X
-- encodeWord codec word = C.evalCodec codec $ do
--     mkX . map Ob . catMaybes <$> mapM (C.maybeEncode fstLens) word
-- 
-- encodeWord' :: (Ord a, Ord b) => Codec a b -> Labeled a b -> (X, Y)
-- encodeWord' codec word = C.evalCodec codec $ do
--     x <- mkX . map Ob . catMaybes <$> mapM (C.maybeEncode fstLens) (obs word)
--     y <- mkY <$> sequence
--     	[ (,) <$> encodeL lb <*> pure pr
-- 	| (lb, pr) <- choice word ]
--     return (x, y)
--   where
--     encodeL y = C.maybeEncode sndLens y >>= \mi -> case mi of
--         -- | TODO: Make corrections !
--         Nothing -> return (Lb (-1)) -- error "encodeWord': unknown label"
--         Just i  -> return (Lb i)
-- 
-- encodeSent :: Ord a => Codec a b -> [[a]] -> Xs
-- encodeSent codec = V.fromList . map (encodeWord codec)
-- 
-- encodeSent' :: (Ord a, Ord b) => Codec a b -> [Labeled a b] -> (Xs, Ys)
-- encodeSent' codec ws =
--     (V.fromList xs, V.fromList ys)
--   where
--     ps = map (encodeWord' codec) ws
--     xs = map fst ps
--     ys = map snd ps
-- 
-- updateCodec :: (Ord a, Ord b) => Codec a b -> [Labeled a b] -> Codec a b
-- updateCodec codec = C.execCodec codec . mapM_ updateWord
-- 
-- mkCodec :: (Ord a, Ord b) => [Labeled a b] -> Codec a b
-- mkCodec = updateCodec (C.empty, C.empty)
-- 
-- updateWord :: (Ord a, Ord b) => Labeled a b -> C.Codec (Codec a b) ()
-- updateWord word = do
--     mapM_ (C.encode' fstLens) (obs word)
--     sequence_ [C.encode sndLens y | (y, _) <- choice word]
