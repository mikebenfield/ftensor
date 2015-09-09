{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE CPP #-}

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
    scalar,

    -- MultiIndex,
    -- inBounds,
    -- unsafeIndex,
    -- index,
    -- maybeIndex,

    -- * Creating
    generate,
    convert,
    tensor,

    -- * Mathematical operations
    add,
    tensorProduct,
    -- unsafeContract,
    -- contract,
    -- changeBasis,
    -- changeBasisAll,
) where

import Control.Monad.ST (runST)
import Data.Proxy
import GHC.Exts (fromListN)
import GHC.TypeLits

import Math.FTensor.Lib.Array hiding (generate, convert, index)
import Math.FTensor.Lib.General
import Math.FTensor.Lib.TypeList
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

scalar :: TensorC a m e => Tensor a '[] e -> e
scalar (Tensor arr) =
    INTERNAL_CHECK
        (A.length arr == 1)
        "scalar"
        (A.length arr)
        $ A.index arr 0

-- * Creating

generate
    :: forall a m e (dims::[Nat]) (multiIndices::[[Nat]]).
    ( TensorC a m e
    , multiIndices ~ AllMultiIndicesInBounds dims
    , KnownType multiIndices [SizedList (Length dims) Int]
    , KnownType (Product dims) Int
    )
    => ((SizedList (Length dims) Int) -> e)
    -> Tensor a dims e
generate f = Tensor $ fromListN len (fmap f lists)
  where
    len :: Int
    len = summon (Proxy::Proxy (Product dims))
    lists :: [SizedList (Length dims) Int]
    lists = summon (Proxy::Proxy multiIndices)

{-# INLINE[1] convert #-}
convert :: (TensorC a m e, TensorC b n e) => Tensor a dims e -> Tensor b dims e
convert (Tensor arr) = Tensor $ A.convert arr

{-# RULES "convert/id" convert = id #-}

tensor :: TensorC a m e => e -> Tensor a '[] e
tensor x = Tensor (A.generate 1 $ const x)

-- * Mathematical operations

add
    :: (Num e, TensorC a m e)
    => Tensor a dims e
    -> Tensor a dims e
    -> Tensor a dims e
add (Tensor arr1) (Tensor arr2) =
    INTERNAL_CHECK
        (A.length arr1 == A.length arr2)
        "add"
        (A.length arr1, A.length arr2)
        $ Tensor $ A.generate
            (A.length arr1)
            (\i -> A.index arr1 i + A.index arr2 i)

tensorProduct
    :: forall a m e (ds1::[Nat]) (ds2::[Nat]).
    ( TensorC a m e
    , Num e
    , KnownType (Product ds1) Int
    , KnownType (Product ds2) Int
    )
    => Tensor a ds1 e
    -> Tensor a ds2 e
    -> Tensor a (Append ds1 ds2) e
tensorProduct (Tensor arr1) (Tensor arr2) = Tensor $ runST $ do
    newArr <- A.new newLen
    let oneRow = \i ->
         let x = A.index arr1 i
             i' = i*len2
         in
         mapM_
            (\j -> A.write newArr (j+i') (x * A.index arr2 j))
            [0 .. len2-1]
    mapM_ oneRow [0..len1-1]
    freeze newArr
  where
    newLen = len1 * len2
    len1 :: Int
    len1 = summon (Proxy::Proxy (Product ds1))
    len2 :: Int
    len2 = summon (Proxy::Proxy (Product ds2))

-- -- unsafeContract = undefined
-- -- contract = undefined
-- -- changeBasis = undefined
-- -- changeBasisAll = undefined