{-# LANGUAGE MagicHash #-}

module Math.FTensor.InternalArray (
    ArrayC(..),
    PrimArray(..),
    Array(..),
    MutablePrimArray(..),
    MutableArray(..),
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

class (IsList a) => ArrayC a m | a -> m, m -> a where
    index :: a -> Int -> Item a
    length :: a -> Int

    new :: Int -> ST s (m s)
    mLength :: m s -> Int
    read :: m s -> Int -> ST s (Item a)
    write :: m s -> Int -> Item a -> ST s ()
    copy :: m s -> Int -> a -> Int -> Int -> ST s ()
    copyM :: m s -> Int -> m s -> Int -> Int -> ST s ()
    freeze :: m s -> ST s a

arrayFromList :: ArrayC a m => [Item a] -> a
arrayFromList lst = arrayFromListN (Prelude.length lst) lst

arrayFromListN :: ArrayC a m => Int -> [Item a] -> a
arrayFromListN len lst = unfoldN len f lst
  where
    f [] = error "fromListN: can't happen"
    f (x:xs) = (x, xs)

arrayToList :: ArrayC a m => a -> [Item a]
arrayToList arr = loop 0
  where
    len = length arr
    loop n
      | n >= len = []
      | otherwise = index arr n : loop (n+1)

newtype PrimArray e = PrimArray BA.ByteArray

newtype MutablePrimArray e s = MutablePrimArray (BA.MutableByteArray s)

instance Prim e => IsList (PrimArray e) where
    type Item (PrimArray e) = e
    fromList = arrayFromList
    fromListN = arrayFromListN
    toList = arrayToList

instance Prim e => ArrayC (PrimArray e) (MutablePrimArray e) where
    {-# INLINE index #-}
    index = \(PrimArray ba) i -> BA.indexByteArray ba i

    {-# INLINE length #-}
    length = \(PrimArray ba) -> 
        div (BA.sizeofByteArray ba) $ I# (sizeOf# (undefined :: e))

    {-# INLINE new #-}
    new = \i -> do
        ba <- BA.newByteArray (i * I# (sizeOf# (undefined::e)))
        return $ MutablePrimArray ba

    {-# INLINE mLength #-}
    mLength = \(MutablePrimArray mba) -> BA.sizeofMutableByteArray mba

    {-# INLINE read #-}
    read = \(MutablePrimArray mba) -> BA.readByteArray mba

    {-# INLINE write #-}
    write = \(MutablePrimArray mba) -> BA.writeByteArray mba

    {-# INLINE copy #-}
    copy = \(MutablePrimArray mba) i (PrimArray ba) j k ->
        let sz = I# (sizeOf# (undefined::e))
        in
        BA.copyByteArray mba (i*sz) ba (j*sz) (k*sz)

    {-# INLINE copyM #-}
    copyM = \(MutablePrimArray mba) i (MutablePrimArray mba') j k ->
        let sz = I# (sizeOf# (undefined::e))
        in
        BA.copyMutableByteArray mba (i*sz) mba' (j*sz) (k*sz)

    {-# INLINE freeze #-}
    freeze = \(MutablePrimArray mba) ->
        BA.unsafeFreezeByteArray mba >>= return . PrimArray

newtype MutableArray a s = MutableArray (A.MutableArray s a)

newtype Array e = Array (A.Array e)

instance IsList (Array e) where
    type Item (Array e) = e
    fromList = arrayFromList
    fromListN = arrayFromListN
    toList = arrayToList

instance ArrayC (Array e) (MutableArray e) where
    {-# INLINE index #-}
    index = \(Array a) -> A.indexArray a

    {-# INLINE length #-}
    -- for some reason Data.Primitive.Array doesn't expose a length function
    length = \(Array (A.Array x)) -> I# (sizeofArray# x)

    {-# INLINE new #-}
    new = \i -> do
        ma <- A.newArray i undefined
        return $ MutableArray ma

    {-# INLINE mLength #-}
    mLength = \(MutableArray (A.MutableArray ma)) ->
        I# (sizeofMutableArray# ma)

    {-# INLINE read #-}
    read = \(MutableArray ma) -> A.readArray ma

    {-# INLINE write #-}
    write = \(MutableArray ma) -> A.writeArray ma

    {-# INLINE copy #-}
    copy = \(MutableArray ma) i (Array a) -> A.copyArray ma i a

    {-# INLINE copyM #-}
    copyM = \(MutableArray ma) i (MutableArray ma') ->
        A.copyMutableArray ma i ma'

    {-# INLINE freeze #-}
    freeze = \(MutableArray ma) -> A.unsafeFreezeArray ma >>= return . Array

generate :: ArrayC a m => Int -> (Int -> Item a) -> a
generate len f = runST $ do
    newArr <- new len
    let at = \i -> write newArr i (f i)
    sequence_ $ map at [0..len-1]
    freeze newArr

unfoldN :: ArrayC a m => Int -> (b -> (Item a, b)) -> b -> a
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
convert :: (ArrayC a m, ArrayC b n, Item a ~ Item b) => a -> b
convert arr = generate (length arr) (index arr)

{-# RULES "convert/id" convert = id #-}
