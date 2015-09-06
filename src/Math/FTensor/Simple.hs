{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-} -- for TensorSizedList

module Math.FTensor.Simple (
    -- * Types
    Tensor(..),
    TensorBoxed(..),
    TensorPrim(..),

    -- * Indexing
    MultiIndex,
    inBounds,
    unsafeIndex,
    index,
    maybeIndex,

    -- * Creating
    generate,
    convert,

    -- * Mathematical operations
    add,
    tensorProduct,
    unsafeContract,
    contract,
    changeBasis,
    changeBasisAll,
) where

import Data.Foldable (Foldable, foldr, foldl')
import Data.Proxy
import GHC.Exts (IsList(..))
import GHC.TypeLits

import Math.FTensor.Lib.Array hiding (generate, convert, index)
import Math.FTensor.Lib.Simple
import Math.FTensor.Internal.TaggedList
import Math.FTensor.SizedList
import qualified Math.FTensor.Lib.Array as A
import qualified Math.FTensor.Internal.Check

#include "ftensor.h"

-- * Types

class
    ( Array (ArrayT t e) (MutableArrayT t e)
    , Item (ArrayT t e) ~ e
    )
    => Tensor t e where

    type ArrayT t e
    type MutableArrayT t e :: * -> *
    contents
        :: (t dim slotCount e)
        -> TensorContents dim slotCount (ArrayT t e) e
    fromContents
        :: TensorContents dim slotCount (ArrayT t e) e
        -> (t dim slotCount e)

data TensorBoxed (dim::Nat) (slotCount::Nat) e where
    ZeroBoxed :: !e -> TensorBoxed dim 0 e
    PositiveBoxed :: {-# UNPACK #-} !(ArrayBoxed e) -> TensorBoxed dim (n+1) e

instance Functor (TensorBoxed dim slotCount) where
    fmap f (ZeroBoxed x) = ZeroBoxed $ f x
    fmap f (PositiveBoxed arr) =
        PositiveBoxed $ generate (A.length arr) (f . A.index arr)

instance TensorIsListConstraint dim slotCount scm1 e
    => IsList (TensorBoxed dim slotCount e) where

    type Item (TensorBoxed dim slotCount e) = IsListContents dim slotCount e

    {-# INLINE fromList #-}
    fromList = fromContents . fromList

    {-# INLINE fromListN #-}
    fromListN = \i lst -> fromContents $ fromListN i lst

    {-# INLINE toList #-}
    toList = toList . contents

instance Tensor TensorBoxed e where
    type ArrayT TensorBoxed e = ArrayBoxed e
    type MutableArrayT TensorBoxed e = MutableArrayBoxed e

    {-# INLINE contents #-}
    contents (ZeroBoxed x) = Zero x
    contents (PositiveBoxed arr) = Positive arr

    {-# INLINE fromContents #-}
    fromContents (Zero x) = ZeroBoxed x
    fromContents (Positive arr) = PositiveBoxed arr

data TensorPrim (dim::Nat) (slotCount::Nat)  e where
    ZeroPrim :: !e -> TensorPrim dim 0 e
    PositivePrim :: {-# UNPACK #-} !(ArrayPrim e) -> TensorPrim dim (n+1) e

instance (TensorIsListConstraint dim slotCount scm1 e, Prim e)
    => IsList (TensorPrim dim slotCount e) where

    type Item (TensorPrim dim slotCount e) =
        IsListContents dim slotCount e

    {-# INLINE fromList #-}
    fromList = fromContents . fromList

    {-# INLINE fromListN #-}
    fromListN i lst = fromContents $ fromListN i lst

    {-# INLINE toList #-}
    toList = toList . contents

instance Prim e => Tensor TensorPrim e where
    type ArrayT TensorPrim e = ArrayPrim e
    type MutableArrayT TensorPrim e = MutableArrayPrim e

    {-# INLINE contents #-}
    contents (ZeroPrim x) = Zero x
    contents (PositivePrim arr) = Positive arr

    {-# INLINE fromContents #-}
    fromContents (Zero x) = ZeroPrim x
    fromContents (Positive arr) = PositivePrim arr

-- * Indexing

type MultiIndex (slotCount::Nat) = SizedList slotCount Int

inBounds :: Int -> MultiIndex slotCount -> Bool
inBounds _ N = True
inBounds dim (i:-is)
  | i >= 0 && i < dim = inBounds dim is
  | otherwise = False

multiIndexToI :: Int -> MultiIndex slotCount -> Int
multiIndexToI dim multiIndex = foldl' f 0 multiIndex
  where
    f accum new = dim*accum + new

unsafeIndex
    :: forall t e dim slotCount
    . (Tensor t e, KnownNat dim)
    => t dim slotCount e
    -> MultiIndex slotCount
    -> e
unsafeIndex t multiIndex = case contents t of
    Zero x -> x
    Positive arr -> UNSAFE_CHECK
        (inBounds dim' multiIndex)
        "unsafeIndex"
        (dim', multiIndex)
        A.index arr (multiIndexToI dim' multiIndex)
  where
    dim' :: Int
    dim' = fromInteger . natVal $ (Proxy::Proxy dim)

index
    :: forall t e dim slotCount
    . (Tensor t e, KnownNat dim)
    => t dim slotCount e
    -> MultiIndex slotCount
    -> e
index t multiIndex = BOUNDS_CHECK
    (inBounds dim' multiIndex)
    "index"
    (dim', multiIndex)
    $ unsafeIndex t multiIndex
  where
    dim' = fromInteger . natVal $ (Proxy::Proxy dim)

maybeIndex
    :: forall t e dim slotCount
    . (Tensor t e, KnownNat dim)
    => t dim slotCount e
    -> MultiIndex slotCount
    -> Maybe e
maybeIndex t multiIndex
  | inBounds dim' multiIndex = Just $ unsafeIndex t multiIndex
  | otherwise = Nothing
  where
    dim' = fromInteger . natVal $ (Proxy::Proxy dim)

-- * Creating

generate = undefined

{-# INLINE[1] convert #-}
convert :: (Tensor t e, Tensor u e) => t dim slotCount e -> u dim slotCount e
convert t = fromContents $ case contents t of
    Positive arr -> Positive $ A.convert arr
    Zero x -> Zero x

{-# RULES "convert/id" convert = id #-}

-- * Mathematical operations

add = undefined
tensorProduct = undefined
unsafeContract = undefined
contract = undefined
changeBasis = undefined
changeBasisAll = undefined
