{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Math.FTensor.InternalArray (
    ArrayC(..),
    PrimArray(..),
    Array(..),
    MutablePrimArray(..),
    MutableArray(..),
) where

import Control.Monad.ST (ST)
import GHC.Exts (Int(..), (*#), sizeofArray#, sizeofMutableArray#)

import Data.Primitive.Types (Prim(..))
import Data.Primitive.Array (Array(..))
import qualified Data.Primitive.Array as A
import qualified Data.Primitive.ByteArray as BA

class ArrayC a m | a -> m, m -> a where
    type Elem a

    index :: a -> Int -> Elem a
    length :: a -> Int

    new :: Int -> ST s (m s)
    mLength :: m s -> Int
    read :: m s -> Int -> ST s (Elem a)
    write :: m s -> Int -> Elem a -> ST s ()
    copy :: m s -> Int -> a -> Int -> Int -> ST s ()
    copyM :: m s -> Int -> m s -> Int -> Int -> ST s ()
    freeze :: m s -> ST s a

newtype PrimArray e = PrimArray BA.ByteArray

newtype MutablePrimArray e s = MutablePrimArray (BA.MutableByteArray s)

instance Prim e => ArrayC (PrimArray e) (MutablePrimArray e) where
    type Elem (PrimArray e) = e

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

-- sadly it seems I need this newtype just for swapping the order of the
-- type parameters
newtype MutableArray a s = MutableArray (A.MutableArray s a)

instance ArrayC (Array e) (MutableArray e) where
    type Elem (Array e) = e

    {-# INLINE index #-}
    index = A.indexArray

    {-# INLINE length #-}
    length = \(Array x) -> I# (sizeofArray# x)

    {-# INLINE new #-}
    new = \i -> do
        ma <- A.newArray i undefined
        return $ MutableArray ma

    {-# INLINE mLength #-}
    -- for some reason Data.Primitive.Array doesn't expose a length function
    mLength = \(MutableArray (A.MutableArray ma)) ->
        I# (sizeofMutableArray# ma)

    {-# INLINE read #-}
    read = \(MutableArray ma) -> A.readArray ma

    {-# INLINE write #-}
    write = \(MutableArray ma) -> A.writeArray ma

    {-# INLINE copy #-}
    copy = \(MutableArray ma) -> A.copyArray ma

    {-# INLINE copyM #-}
    copyM = \(MutableArray ma) i (MutableArray ma') ->
        A.copyMutableArray ma i ma'

    {-# INLINE freeze #-}
    freeze = \(MutableArray ma) -> A.unsafeFreezeArray ma
