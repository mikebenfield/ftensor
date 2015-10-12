{-# LANGUAGE CPP #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MagicHash #-}

module Math.FTensor.Lib.Array (
    Array(..),
    index,
    read,
    write,
    ArrayPrim,
    ArrayBoxed,
    Prim(..),
    generate,
    unfoldN,
    convert,
    map,
) where

#include "ftensor.h"

import Prelude hiding (length, read, map)
import qualified Prelude

import Control.Monad (liftM)
import Control.Monad.ST (ST, runST)
import Data.STRef
import GHC.Exts (Int(..), sizeofArray#, sizeofMutableArray#, IsList(..))

import Control.DeepSeq
import Data.Primitive.Types (Prim(..))
import qualified Data.Primitive.Array as A
import qualified Data.Primitive.ByteArray as BA

import Math.FTensor.Internal.Check

class (IsList (a e), Item (a e) ~ e) => Array a e where
    data Mutable s a e
    basicIndex :: a e -> Int -> e
    length :: a e -> Int
    new :: Int -> ST s (Mutable s a e)
    replicate :: Int -> e -> a e
    replicate i x = generate i (const x)
    mLength :: Mutable s a e -> Int
    basicRead :: Mutable s a e -> Int -> ST s e
    basicWrite :: Mutable s a e -> Int -> e -> ST s ()
    copy :: Mutable s a e -> Int -> a e -> Int -> Int -> ST s ()
    copyM :: Mutable s a e -> Int -> Mutable s a e -> Int -> Int -> ST s ()
    freeze :: Mutable s a e -> ST s (a e)

{-# INLINE index #-}
index :: Array a e => a e -> Int -> e
index = \a i ->
    UNSAFE_CHECK
        (0 <= i && i < length a)
        "index"
        (i, length a)
        $ basicIndex a i

{-# INLINE read #-}
read :: Array a e => Mutable s a e -> Int -> ST s e
read = \m i ->
    UNSAFE_CHECK
        (0 <= i && i < mLength m)
        "read"
        (i, mLength m)
        $ basicRead m i

{-# INLINE write #-}
write :: Array a e => Mutable s a e -> Int -> e -> ST s ()
write = \m i x ->
    UNSAFE_CHECK
        (0 <= i && i < mLength m)
        "write"
        (i, mLength m)
        $ basicWrite m i x

arrayFromList :: Array a e => [e] -> a e
arrayFromList lst = arrayFromListN (Prelude.length lst) lst

arrayFromListN :: Array a e => Int -> [e] -> a e
arrayFromListN len lst = unfoldN len f lst
  where
    f [] = error "fromListN: can't happen"
    f (x:xs) = (x, xs)

arrayToList :: Array a e => a e -> [e]
arrayToList arr = loop 0
  where
    loop i
      | i < length arr = index arr i : loop (i+1)
      | otherwise = []

arrayEq :: (Eq e, Array a e) => a e -> a e -> Bool
arrayEq lhs rhs = len == length rhs && all f [0..len-1]
  where
    f i = index rhs i == index lhs i
    len = length lhs

newtype ArrayPrim e = ArrayPrim BA.ByteArray

instance NFData (ArrayPrim e) where
    rnf (ArrayPrim a) = a `seq` ()

instance (Eq e, Prim e) => Eq (ArrayPrim e) where
    (==) = arrayEq

instance (Show e, Prim e) => Show (ArrayPrim e) where
    show arr = "fromList " ++ show (toList arr)

instance Prim e => IsList (ArrayPrim e) where
    type Item (ArrayPrim e) = e
    fromList = arrayFromList
    fromListN = arrayFromListN
    toList = arrayToList

instance Prim e => Array ArrayPrim e where
    newtype Mutable s ArrayPrim e = MutableArrayPrim (BA.MutableByteArray s)

    {-# INLINE basicIndex #-}
    basicIndex = \(ArrayPrim ba) i -> BA.indexByteArray ba i

    {-# INLINE length #-}
    length = \(ArrayPrim ba) -> 
        div (BA.sizeofByteArray ba) $ I# (sizeOf# (undefined :: e))

    {-# INLINE new #-}
    new = \i ->
        MutableArrayPrim <$> BA.newByteArray (i * I# (sizeOf# (undefined::e)))

    {-# INLINE replicate #-}
    replicate = \i x -> runST $ do
        mba <- BA.newByteArray (i * I# (sizeOf# (undefined::e)))
        BA.setByteArray mba 0 i x
        liftM ArrayPrim $ BA.unsafeFreezeByteArray mba

    {-# INLINE mLength #-}
    mLength = \(MutableArrayPrim mba) -> BA.sizeofMutableByteArray mba

    {-# INLINE basicRead #-}
    basicRead = \(MutableArrayPrim mba) -> BA.readByteArray mba

    {-# INLINE basicWrite #-}
    basicWrite = \(MutableArrayPrim mba) -> BA.writeByteArray mba

    {-# INLINE copy #-}
    copy = \(MutableArrayPrim mba) i (ArrayPrim ba) j k ->
        let sz = I# (sizeOf# (undefined::e))
        in
        BA.copyByteArray mba (i*sz) ba (j*sz) (k*sz)

    {-# INLINE copyM #-}
    copyM = \(MutableArrayPrim mba) i (MutableArrayPrim mba') j k ->
        let sz = I# (sizeOf# (undefined::e))
        in
        BA.copyMutableByteArray mba (i*sz) mba' (j*sz) (k*sz)

    {-# INLINE freeze #-}
    freeze = \(MutableArrayPrim mba) ->
        ArrayPrim <$> BA.unsafeFreezeByteArray mba

newtype ArrayBoxed e = ArrayBoxed (A.Array e)

instance Eq e => Eq (ArrayBoxed e) where
    (==) = arrayEq

instance NFData e => NFData (ArrayBoxed e) where
    rnf arr = f 0
      where
        len = length arr
        f i
          | i < len = rnf (index arr i) `seq` f (i+1)
          | otherwise = ()

instance Show e => Show (ArrayBoxed e) where
    show arr = "fromList " ++ show (toList arr)

instance IsList (ArrayBoxed e) where
    type Item (ArrayBoxed e) = e
    fromList = arrayFromList
    fromListN = arrayFromListN
    toList = arrayToList

instance Foldable ArrayBoxed where
    foldr f init arr = loop (length arr - 1) init
      where
        loop i accum
          | i < 0 = accum
          | otherwise = loop (i-1) $ f (index arr i) accum

instance Traversable ArrayBoxed where
    traverse f array = fromListN (length array) <$> traverse f (toList array)

instance Functor ArrayBoxed where
    fmap = map

instance Array ArrayBoxed e where
    newtype Mutable s ArrayBoxed e = MutableArrayBoxed (A.MutableArray s e)

    {-# INLINE basicIndex #-}
    basicIndex = \(ArrayBoxed a) -> A.indexArray a

    {-# INLINE length #-}
    -- for some reason Data.Primitive.Array doesn't expose a length function
    length = \(ArrayBoxed (A.Array x)) -> I# (sizeofArray# x)

    {-# INLINE new #-}
    new = \i -> MutableArrayBoxed <$> A.newArray i
        (error "uninitialized array element")

    {-# INLINE replicate #-}
    replicate = \i x -> runST $
        A.newArray i x >>= (freeze . MutableArrayBoxed)

    {-# INLINE mLength #-}
    mLength = \(MutableArrayBoxed (A.MutableArray ma)) ->
        I# (sizeofMutableArray# ma)

    {-# INLINE basicRead #-}
    basicRead = \(MutableArrayBoxed ma) -> A.readArray ma

    {-# INLINE basicWrite #-}
    basicWrite = \(MutableArrayBoxed ma) -> A.writeArray ma

    {-# INLINE copy #-}
    copy = \(MutableArrayBoxed ma) i (ArrayBoxed a) -> A.copyArray ma i a

    {-# INLINE copyM #-}
    copyM = \(MutableArrayBoxed ma) i (MutableArrayBoxed ma') ->
        A.copyMutableArray ma i ma'

    {-# INLINE freeze #-}
    freeze = \(MutableArrayBoxed ma) -> ArrayBoxed <$> A.unsafeFreezeArray ma

generate :: Array a e => Int -> (Int -> e) -> a e
generate len f = runST $ do
    newArr <- new len
    let at i = write newArr i (f i)
    mapM_ at [0..len-1]
    freeze newArr

unfoldN :: Array a e => Int -> (b -> (e, b)) -> b -> a e
unfoldN len f init = runST $ do
    newArr <- new len
    ref <- newSTRef init
    let at i = do
            oldB <- readSTRef ref
            let (elem, newB) = f oldB
            writeSTRef ref newB
            write newArr i elem
    mapM_  at [0..len-1]
    freeze newArr

{-# INLINE[1] convert #-}
convert :: (Array a e, Array b e) => a e -> b e
convert arr = generate (length arr) (index arr)

{-# RULES "convert/id" convert = id #-}

map :: (Array a e, Array b d) => (e -> d) -> a e -> b d
map f arr = generate (length arr) (f . index arr)
