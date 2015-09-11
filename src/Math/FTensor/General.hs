{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE UndecidableInstances #-} -- for ContractedDims, IsList

module Math.FTensor.General (
    -- * Types
    Tensor(..),
    TensorC,
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
    scale,
    add,
    minus,
    tensorProduct,
    contract,
    trace,
    dot,
    -- changeBasis,
    -- changeBasisAll,
) where

import Control.Monad.ST (runST)
import Data.Proxy
import GHC.Exts (IsList(..))
import Data.STRef
import GHC.TypeLits

import Control.DeepSeq

import Math.FTensor.Internal.TaggedList
import Math.FTensor.Lib.Array hiding (generate, convert, index)
import Math.FTensor.Lib.General
import Math.FTensor.Lib.TypeList
import Math.FTensor.SizedList
import qualified Math.FTensor.Lib.Array as A
import qualified Math.FTensor.Internal.Check

import Math.FTensor.Algebra

#include "ftensor.h"

-- * Types

newtype Tensor a (dims::[Nat]) e = Tensor (a e)
  deriving (Eq, Show, Functor, Traversable, Foldable)

type TensorC a m e = (Array (a e) m, Item (a e) ~ e)

instance NFData (a e) => NFData (Tensor a dims e) where
    rnf (Tensor arr) = rnf arr

instance
    ( dims ~ (d ': ds)
    , TensorC a m e
    , IsList (TaggedList dims e)
    , Item (TaggedList dims e) ~ IsListItem dims e
    , KnownNat (Product dims)
    )
    => IsList (Tensor a dims e) where

    type Item (Tensor a dims e) = IsListItem dims e

    fromList lst = fromListN (Prelude.length lst) lst

    fromListN len lst =
        let arrayLen = natIntVal (Proxy::Proxy (Product dims))
            (lst'::TaggedList dims e) = fromListN len lst
        in
        Tensor $ runST $ do
            newArr <- new arrayLen
            idx <- newSTRef (0::Int)
            let at = \x -> do
                 i <- readSTRef idx
                 write newArr i x
                 writeSTRef idx (i+1)
            mapM_ at lst'
            freeze newArr

    toList = undefined

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

scale
    :: (Multiplicative e, TensorC a m e)
    => Tensor a dims e
    -> e
    -> Tensor a dims e
scale (Tensor arr) factor =
    Tensor $ A.generate (A.length arr) ((*.factor) . A.index arr)

add
    :: (Additive e, TensorC a m e)
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
            (\i -> A.index arr1 i +. A.index arr2 i)

minus
    :: (WithNegatives e, TensorC a m e)
    => Tensor a dims e
    -> Tensor a dims e
    -> Tensor a dims e
minus (Tensor arr1) (Tensor arr2) =
    INTERNAL_CHECK
        (A.length arr1 == A.length arr2)
        "minus"
        (A.length arr1, A.length arr2)
        $ Tensor $ A.generate
            (A.length arr1)
            (\i -> A.index arr1 i -. A.index arr2 i)

tensorProduct
    :: forall a m e (ds1::[Nat]) (ds2::[Nat]).
    ( TensorC a m e
    , Multiplicative e
    , KnownType (Product ds1) Int
    , KnownType (Product ds2) Int
    )
    => Tensor a ds1 e
    -> Tensor a ds2 e
    -> Tensor a (ds1 ++ ds2) e
tensorProduct (Tensor arr1) (Tensor arr2) = Tensor $ runST $ do
    newArr <- A.new newLen
    let oneRow = \i ->
         let x = A.index arr1 i
             i' = i*len2
         in
         mapM_
            (\j -> A.write newArr (j+i') (x *. A.index arr2 j))
            [0 .. len2-1]
    mapM_ oneRow [0..len1-1]
    freeze newArr
  where
    newLen = len1 * len2
    len1 :: Int
    len1 = summon (Proxy::Proxy (Product ds1))
    len2 :: Int
    len2 = summon (Proxy::Proxy (Product ds2))

contract_
    :: (Additive (Item a), A.Array a m)
    => a -> Int -> Int -> Int -> Int -> a
contract_ arr length iDim ijOffset othersOffset =
    A.generate length (sumStartingAt . (*othersOffset))
  where
    sumStartingAt i = f (i+ijOffset*iDim) (i+ijOffset) (A.index arr i)
    f !max !idx !sum
      | idx < max = f max (idx+ijOffset) (sum +. A.index arr idx)
      | otherwise = sum

contract
    :: forall a m e (dims::[Nat]) (i::Nat) (j::Nat) (newDims::[Nat])
        (newLen::Nat) (iOffset::Nat) (jOffset::Nat) (othersOffset::Nat).
    ( i+1 <= j
    , j+1 <= Length dims
    , (dims !! i) ~ (dims !! j)
    , newDims ~ ContractedDims dims i j
    , newLen ~ Product newDims
    , othersOffset ~ (Sum (Offsets dims) - iOffset - jOffset)
    , iOffset ~ (Product (Drop (i+1) dims))
    , jOffset ~ (Product (Drop (j+1) dims))
    , KnownType (dims !! i) Int
    , KnownType (iOffset+jOffset) Int
    , KnownType newLen Int
    , KnownType othersOffset Int
    , Additive e
    , TensorC a m e
    )
    => Tensor a dims e
    -> Proxy i
    -> Proxy j
    -> Tensor a newDims e
contract (Tensor arr) _ _ = Tensor $ contract_ arr
    (summon (Proxy::Proxy newLen))
    (summon (Proxy::Proxy (dims !! i)))
    (summon (Proxy::Proxy (iOffset+jOffset)))
    (summon (Proxy::Proxy othersOffset))

type family ContractedDims (xs::[Nat]) (i::Nat) (j::Nat) :: [Nat] where
    ContractedDims xs i j = Delete (Delete xs j) i

type family Offsets (dims::[Nat]) :: [Nat] where
    Offsets '[] = '[]
    Offsets (dim ': dims) = Offsets_ dims

type family Offsets_ (dims::[Nat]) :: [Nat] where
    Offsets_ '[] = '[1]
    Offsets_ (dim ': dims) = (dim * Head (Offsets_ dims)) ': Offsets_ dims

trace
    :: forall a m e (dim::Nat)
    . (KnownType dim Int, TensorC a m e, Additive e)
    => Tensor a '[dim, dim] e
    -> e
trace (Tensor arr) = f dim1 (A.index arr 0)
  where
    f !i !sum
      | i < len = f (i + dim1) (sum +. A.index arr i)
      | otherwise = sum
    len = A.length arr
    dim1 = 1 + summon (Proxy::Proxy dim)

dot :: (Num e, TensorC a m e) => Tensor a '[dim] e -> Tensor a '[dim] e -> e
dot (Tensor arr1) (Tensor arr2) =
    INTERNAL_CHECK
        (len1 == len2)
        "dot"
        (len1, len2)
        f 0 0
  where
    f !i !sum
      | i < len1 = f (i+1) (sum + A.index arr1 i * A.index arr2 i)
      | otherwise = sum
    len1 = A.length arr1
    len2 = A.length arr2

instance (Additive e, TensorC a m e) => Additive (Tensor a dims e) where
    {-# INLINE (+.) #-}
    (+.) = add

instance
    ( WithZero e
    , TensorC a m e
    , KnownType (Product dims) Int
    )
    => WithZero (Tensor a dims e) where

    {-# INLINE zero #-}
    zero = Tensor $ A.replicate (summon (Proxy::Proxy (Product dims))) zero

instance
    ( WithNegatives e
    , WithZero (Tensor a dims e)
    , TensorC a m e
    )
    => WithNegatives (Tensor a dims e) where

    {-# INLINE neg #-}
    neg = \(Tensor arr) -> Tensor $ A.map neg arr

    {-# INLINE (-.) #-}
    (-.) = minus

instance (Multiplicative e, TensorC a m e)
    => WithScalars (Tensor a dims e) where

    type Scalar (Tensor a dims e) = e

    {-# INLINE (*:) #-}
    (*:) = flip scale

instance (Multiplicative e, TensorC a m e)
    => Multiplicative (Tensor a '[] e) where

    {-# INLINE (*.) #-}
    lhs *. rhs = tensor (scalar lhs *. scalar rhs)

instance (WithOne e, TensorC a m e)
    => WithOne (Tensor a '[] e) where

    {-# INLINE one #-}
    one = tensor one

instance (WithReciprocals e, TensorC a m e)
    => WithReciprocals (Tensor a '[] e) where

    {-# INLINE inv #-}
    inv = tensor . inv . scalar

    {-# INLINE (/.) #-}
    lhs /. rhs = tensor (scalar lhs /. scalar rhs)

-- changeBasis = undefined
-- changeBasisAll = undefined
