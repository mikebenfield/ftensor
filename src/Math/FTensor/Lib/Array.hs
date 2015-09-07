{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MagicHash #-}

module Math.FTensor.Lib.Array (
    Array(..),
    ArrayPrim(..),
    ArrayBoxed(..),
    MutableArrayPrim(..),
    MutableArrayBoxed(..),
    Prim(..),
    generate,
    unfoldN,
    convert,
) where

import Prelude hiding (length)
import qualified Prelude

import Control.Monad.ST (ST, runST)
import Data.Proxy
import Data.STRef
import GHC.Exts (Int(..), sizeofArray#, sizeofMutableArray#, IsList(..))

import Data.Primitive.Types (Prim(..))
import qualified Data.Primitive.Array as A
import qualified Data.Primitive.ByteArray as BA

class (IsList a) => Array a m | a -> m, m -> a where
    index :: a -> Int -> Item a
    length :: a -> Int

    new :: Int -> ST s (m s)
    mLength :: m s -> Int
    read :: m s -> Int -> ST s (Item a)
    write :: m s -> Int -> Item a -> ST s ()
    copy :: m s -> Int -> a -> Int -> Int -> ST s ()
    copyM :: m s -> Int -> m s -> Int -> Int -> ST s ()
    freeze :: m s -> ST s a

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
    len = length arr
    loop n
      | n >= len = []
      | otherwise = index arr n : loop (n+1)

newtype ArrayPrim e = ArrayPrim BA.ByteArray

newtype MutableArrayPrim e s = MutableArrayPrim (BA.MutableByteArray s)

instance Prim e => IsList (ArrayPrim e) where
    type Item (ArrayPrim e) = e
    fromList = arrayFromList
    fromListN = arrayFromListN
    toList = arrayToList

instance Prim e => Array (ArrayPrim e) (MutableArrayPrim e) where
    {-# INLINE index #-}
    index = \(ArrayPrim ba) i -> BA.indexByteArray ba i

    {-# INLINE length #-}
    length = \(ArrayPrim ba) -> 
        div (BA.sizeofByteArray ba) $ I# (sizeOf# (undefined :: e))

    {-# INLINE new #-}
    new = \i -> do
        ba <- BA.newByteArray (i * I# (sizeOf# (undefined::e)))
        return $ MutableArrayPrim ba

    {-# INLINE mLength #-}
    mLength = \(MutableArrayPrim mba) -> BA.sizeofMutableByteArray mba

    {-# INLINE read #-}
    read = \(MutableArrayPrim mba) -> BA.readByteArray mba

    {-# INLINE write #-}
    write = \(MutableArrayPrim mba) -> BA.writeByteArray mba

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
        BA.unsafeFreezeByteArray mba >>= return . ArrayPrim

newtype MutableArrayBoxed a s = MutableArrayBoxed (A.MutableArray s a)

newtype ArrayBoxed e = ArrayBoxed (A.Array e)

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
    fmap f array = generate (length array) (f . index array)

instance Array (ArrayBoxed e) (MutableArrayBoxed e) where
    {-# INLINE index #-}
    index = \(ArrayBoxed a) -> A.indexArray a

    {-# INLINE length #-}
    -- for some reason Data.Primitive.Array doesn't expose a length function
    length = \(ArrayBoxed (A.Array x)) -> I# (sizeofArray# x)

    {-# INLINE new #-}
    new = \i -> do
        ma <- A.newArray i undefined
        return $ MutableArrayBoxed ma

    {-# INLINE mLength #-}
    mLength = \(MutableArrayBoxed (A.MutableArray ma)) ->
        I# (sizeofMutableArray# ma)

    {-# INLINE read #-}
    read = \(MutableArrayBoxed ma) -> A.readArray ma

    {-# INLINE write #-}
    write = \(MutableArrayBoxed ma) -> A.writeArray ma

    {-# INLINE copy #-}
    copy = \(MutableArrayBoxed ma) i (ArrayBoxed a) -> A.copyArray ma i a

    {-# INLINE copyM #-}
    copyM = \(MutableArrayBoxed ma) i (MutableArrayBoxed ma') ->
        A.copyMutableArray ma i ma'

    {-# INLINE freeze #-}
    freeze = \(MutableArrayBoxed ma) ->
        A.unsafeFreezeArray ma >>= return . ArrayBoxed

generate :: Array a m => Int -> (Int -> Item a) -> a
generate len f = runST $ do
    newArr <- new len
    let at = \i -> write newArr i (f i)
    sequence_ $ map at [0..len-1]
    freeze newArr

unfoldN :: Array a m => Int -> (b -> (Item a, b)) -> b -> a
unfoldN len f init = runST $ do
    newArr <- new len
    ref <- newSTRef init
    let at = \i -> do
         oldB <- readSTRef ref
         let (elem, newB) = f oldB
         writeSTRef ref newB
         write newArr i elem
    sequence_ $ map at [0..len-1]
    freeze newArr

{-# INLINE[1] convert #-}
convert :: (Array a m, Array b n, Item a ~ Item b) => a -> b
convert arr = generate (length arr) (index arr)

{-# RULES "convert/id" convert = id #-}
