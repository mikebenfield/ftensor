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
    MutableArrayPrim,
    MutableArrayBoxed,
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

class (IsList a) => Array a m | a -> m, m -> a where
    basicIndex :: a -> Int -> Item a
    length :: a -> Int
    new :: Int -> ST s (m s)
    replicate :: Int -> Item a -> a
    replicate i x = generate i (const x)
    mLength :: m s -> Int
    basicRead :: m s -> Int -> ST s (Item a)
    basicWrite :: m s -> Int -> Item a -> ST s ()
    copy :: m s -> Int -> a -> Int -> Int -> ST s ()
    copyM :: m s -> Int -> m s -> Int -> Int -> ST s ()
    freeze :: m s -> ST s a

{-# INLINE index #-}
index :: Array a m => a -> Int -> Item a
index = \a i ->
    UNSAFE_CHECK
        (0 <= i && i < length a)
        "index"
        (i, length a)
        $ basicIndex a i

{-# INLINE read #-}
read :: Array a m => m s -> Int -> ST s (Item a)
read = \m i ->
    UNSAFE_CHECK
        (0 <= i && i < mLength m)
        "read"
        (i, mLength m)
        $ basicRead m i

{-# INLINE write #-}
write :: Array a m => m s -> Int -> Item a -> ST s ()
write = \m i x ->
    UNSAFE_CHECK
        (0 <= i && i < mLength m)
        "write"
        (i, mLength m)
        $ basicWrite m i x

arrayFromList :: Array a m => [Item a] -> a
arrayFromList lst = arrayFromListN (Prelude.length lst) lst

arrayFromListN :: Array a m => Int -> [Item a] -> a
arrayFromListN len lst = unfoldN len f lst
  where
    f [] = error "fromListN: can't happen"
    f (x:xs) = (x, xs)

arrayToList :: Array a m => a -> [Item a]
arrayToList arr = loop 0
  where
    loop i
      | i < length arr = index arr i : loop (i+1)
      | otherwise = []

arrayEq :: (Eq (Item a), Array a m) => a -> a -> Bool
arrayEq lhs rhs = len == length rhs && all f [0..len-1]
  where
    f i = index rhs i == index lhs i
    len = length lhs

newtype ArrayPrim e = ArrayPrim BA.ByteArray

newtype MutableArrayPrim e s = MutableArrayPrim (BA.MutableByteArray s)

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

instance Prim e => Array (ArrayPrim e) (MutableArrayPrim e) where
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

newtype MutableArrayBoxed a s = MutableArrayBoxed (A.MutableArray s a)

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

instance Array (ArrayBoxed e) (MutableArrayBoxed e) where
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

generate :: Array a m => Int -> (Int -> Item a) -> a
generate len f = runST $ do
    newArr <- new len
    let at i = write newArr i (f i)
    mapM_ at [0..len-1]
    freeze newArr

unfoldN :: Array a m => Int -> (b -> (Item a, b)) -> b -> a
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
convert :: (Array a m, Array b n, Item a ~ Item b) => a -> b
convert arr = generate (length arr) (index arr)

{-# RULES "convert/id" convert = id #-}

map :: (Array a m, Array b n) => (Item a -> Item b) -> a -> b
map f arr = generate (length arr) (f . index arr)
