{-|
Module: Math.FTensor.Lib.Array
Copyright: (c) 2015 Michael Benfield
License: BSD-3

Simple, efficient indexed arrays, either boxed or not.

Like other modules under @Math.FTensor.Lib@, casual users of the library should
not need to directly use this module.

The types here are just a small wrapper around @Data.Primitive.Array@ and
@Data.Primitve.ByteArray@ from the @primitive@ package, which for some reason
does not provide a common interface to the two types of arrays.

Array operations are deliberately unsafe, but @index@, @read@, and @write@ will
perform bounds checks when the package is compiled with the Cabal option
-fUnsafeChecks (off by default).
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MagicHash #-}

module Math.FTensor.Lib.Array (
    -- * The @Array@ class and common functions.
    Array(..),
    index,
    read,
    write,
    map,

    -- * Types
    ArrayPrim,
    ArrayBoxed,
    Prim,

    -- * Creating arrays.
    generate,
    unfoldN,
    convert,
    arrayFromList,
    arrayFromListN,
    arrayToList,
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

    -- | Access an element of an immutable array. No bounds check is performed.
    -- Users should use @index@ instead.
    basicIndex :: a e -> Int -> e

    -- | Length of an immutable array.
    length :: a e -> Int

    -- | Create a new mutable array of the given length.
    new :: Int -> ST s (Mutable s a e)

    -- | Create a new immutable array of the given length, filled with the
    -- given element. A default implementation is provided.
    replicate :: Int -> e -> a e
    replicate i x = generate i (const x)

    -- | Length of a mutable array.
    mLength :: Mutable s a e -> Int

    -- | Access an element of an immutable array. No bounds check is performed.
    -- Users should use @read@ instead.
    basicRead :: Mutable s a e -> Int -> ST s e

    -- | Write an element of an immutable array. No bounds check is performed.
    -- Users should use @write@ instead.
    basicWrite :: Mutable s a e -> Int -> e -> ST s ()

    -- | Copy a slice from an immutable array into a mutable array.
    copy
        :: Mutable s a e -- ^ destination
        -> Int -- ^ offset into destination
        -> a e -- ^ source
        -> Int -- ^ offset into source
        -> Int -- ^ number of elements to copy
        -> ST s ()

    -- | Copy a slice from a mutable array into a mutable array.
    copyM
        :: Mutable s a e -- ^ destination
        -> Int -- ^ offset into destination
        -> Mutable s a e -- ^ source
        -> Int -- ^ offset into source
        -> Int -- ^ number of elements to copy
        -> ST s ()

    -- | Convert a mutable array to an immutable one without copying. The
    -- mutable array should not be modified after the conversion.
    freeze :: Mutable s a e -> ST s (a e)

-- | Unsafely access an element of an immutable array. Will perform bounds
-- checks when the package is compiled with the Cabal option -fUnsafeChecks
-- (off by default).
{-# INLINE index #-}
index :: Array a e => a e -> Int -> e
index = \a i ->
    UNSAFE_CHECK
        (0 <= i && i < length a)
        "index"
        (i, length a)
        $ basicIndex a i

-- | Unsafely access an element of a mutable array. Will perform bounds checks
-- when the package is compiled with the Cabal option -fUnsafeChecks (off by
-- default).
{-# INLINE read #-}
read :: Array a e => Mutable s a e -> Int -> ST s e
read = \m i ->
    UNSAFE_CHECK
        (0 <= i && i < mLength m)
        "read"
        (i, mLength m)
        $ basicRead m i

-- | Unsafely write an element of a mutable array. Will perform bounds checks
-- when the package is compiled with the Cabal option -fUnsafeChecks (off by
-- default).
{-# INLINE write #-}
write :: Array a e => Mutable s a e -> Int -> e -> ST s ()
write = \m i x ->
    UNSAFE_CHECK
        (0 <= i && i < mLength m)
        "write"
        (i, mLength m)
        $ basicWrite m i x

-- | To implement @fromList@ in the @IsList@ class.
arrayFromList :: Array a e => [e] -> a e
arrayFromList lst = arrayFromListN (Prelude.length lst) lst

-- | To implement @fromListN@ in the @IsList@ class.
arrayFromListN :: Array a e => Int -> [e] -> a e
arrayFromListN len lst = unfoldN len f lst
  where
    f [] = error "fromListN: can't happen"
    f (x:xs) = (x, xs)

-- | To implement @toList@ in the @IsList@ class.
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

-- | Immutable unboxed arrays of types which are instances of @Prim@. (Note
-- that @Prim@ is from the @primitive@ package; users can implement it to allow
-- additional types to be placed into these arrays.)
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

-- | Immutable boxed arrays.
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

-- | @generate n f@: an array of length @n@ whose element at position @i@
-- is @f i@.
generate :: Array a e => Int -> (Int -> e) -> a e
generate len f = runST $ do
    newArr <- new len
    let at i = write newArr i (f i)
    mapM_ at [0..len-1]
    freeze newArr

-- | @unfoldN n f init@: an array of length @n@, created by repeatedly
-- applying @f@ to seeds starting with @init@.
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

-- | convert an immutable array from one type to another.
{-# INLINE[1] convert #-}
convert :: (Array a e, Array b e) => a e -> b e
convert arr = generate (length arr) (index arr)

{-# RULES "convert/id" convert = id #-}

-- | Not every array type can be a @Functor@, so this is provided.
map :: (Array a e, Array b d) => (e -> d) -> a e -> b d
map f arr = generate (length arr) (f . index arr)
