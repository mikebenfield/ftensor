{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-} -- for TensorSizedList

module Math.FTensor.General (
    -- * Types
    Tensor,
    TensorBoxed,
    TensorPrim,

    -- * Indexing
    pIndex,
    unsafeIndex,
    index,
    maybeIndex,

    -- MultiIndex,
    -- inBounds,
    -- unsafeIndex,
    -- index,
    -- maybeIndex,

    -- * Creating
    generate,
    convert,

    -- -- * Mathematical operations
    -- add,
    -- tensorProduct,
    -- unsafeContract,
    -- contract,
    -- changeBasis,
    -- changeBasisAll,
) where

import Data.Proxy
import GHC.Exts (IsList(..))
import GHC.TypeLits

import Math.FTensor.Lib.Array hiding (generate, convert, index)
import Math.FTensor.Lib.General
import Math.FTensor.Lib.TypeList
import Math.FTensor.Internal.TaggedList
import Math.FTensor.SizedList
import qualified Math.FTensor.Lib.Array as A
import qualified Math.FTensor.Internal.Check

#include "ftensor.h"

-- * Types

type TensorBoxed (dims::[Nat]) e = Tensor ArrayBoxed dims e

type TensorPrim (dims::[Nat]) e = Tensor ArrayPrim dims e

-- * Indexing

pIndex
    :: forall a m e (dims::[Nat]) (multiIndex::[Nat]).
    ( TensorC a m e
    , InBounds dims multiIndex
    , KnownNat (MultiIndexToI dims multiIndex)
    )
    => Tensor a dims e
    -> Proxy multiIndex
    -> e
pIndex (Tensor arr) p = A.index arr (multiIndexToI' (Proxy::Proxy dims) p)

unsafeIndex
    :: forall a m e (dims::[Nat]).
    ( TensorC a m e
    , KnownType dims (SizedList (Length dims) Int)
    )
    => Tensor a dims e
    -> SizedList (Length dims) Int
    -> e
unsafeIndex (Tensor arr) multiIndex =
    UNSAFE_CHECK
        (inBounds p multiIndex)
        "unsafeIndex"
        ((summon p)::SizedList (Length dims) Int, multiIndex)
        $ A.index arr (multiIndexToI p multiIndex)
    where
      p :: Proxy dims
      p = Proxy

index
    :: forall a m e (dims::[Nat]).
    ( TensorC a m e
    , KnownType dims (SizedList (Length dims) Int)
    )
    => Tensor a dims e
    -> SizedList (Length dims) Int
    -> e
index t multiIndex =
    BOUNDS_CHECK
        (inBounds p multiIndex)
        "index"
        ((summon p)::SizedList (Length dims) Int, multiIndex)
        $ unsafeIndex t multiIndex
    where
      p :: Proxy dims
      p = Proxy

maybeIndex
    :: forall a m e (dims::[Nat]).
    ( TensorC a m e
    , KnownType dims (SizedList (Length dims) Int)
    )
    => Tensor a dims e
    -> SizedList (Length dims) Int
    -> Maybe e
maybeIndex t multiIndex
  | inBounds (Proxy::Proxy dims) multiIndex = Just $ unsafeIndex t multiIndex
  | otherwise = Nothing

-- * Creating

generate
    :: forall a m e (dims::[Nat]) (multiIndices::[[Nat]]).
    ( TensorC a m e
    , multiIndices ~ AllMultiIndicesInBounds dims
    , KnownType multiIndices [SizedList (Length dims) Int]
    , KnownType (Product dims) Int
    )
    => Proxy dims
    -> ((SizedList (Length dims) Int) -> e)
    -> Tensor a dims e
generate _ f = Tensor $ fromListN len (fmap f lists)
  where
    len :: Int
    len = summon (Proxy::Proxy (Product dims))
    lists :: [SizedList (Length dims) Int]
    lists = summon (Proxy::Proxy multiIndices)

{-# INLINE[1] convert #-}
convert :: (TensorC a m e, TensorC b n e) => Tensor a dims e -> Tensor b dims e
convert (Tensor arr) = Tensor $ A.convert arr

{-# RULES "convert/id" convert = id #-}

-- -- -- * Mathematical operations

-- -- add = undefined
-- -- tensorProduct = undefined
-- -- unsafeContract = undefined
-- -- contract = undefined
-- -- changeBasis = undefined
-- -- changeBasisAll = undefined
