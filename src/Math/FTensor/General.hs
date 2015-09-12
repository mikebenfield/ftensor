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
    MultiIndex,

    -- * Indexing
    PIndexConstraint,
    pIndex,
    unsafeIndex,
    index,
    maybeIndex,
    scalar,

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

    -- * Type families
    GenerateConstraint,
    TensorProductConstraint,
    ContractConstraint,
    ContractedDims,
) where

import Control.Monad.ST (runST)
import Data.Proxy
import Data.STRef
import GHC.Exts (IsList(..))
import GHC.TypeLits

import Control.DeepSeq

import Math.FTensor.Internal.TaggedList
import qualified Math.FTensor.Internal.Check

import Math.FTensor.Lib.General
import Math.FTensor.Lib.TypeList
import qualified Math.FTensor.Lib.Array as A

import Math.FTensor.Algebra
import Math.FTensor.SizedList

#include "ftensor.h"

-- * Types

newtype Tensor a (dims::[Nat]) e = Tensor (a e)
  deriving (Eq, Show, Functor, Traversable, Foldable)

type TensorC a m e = (A.Array (a e) m, Item (a e) ~ e)

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
        let arrayLen = summon (Proxy::Proxy (Product dims))
            (lst'::TaggedList dims e) = fromListN len lst
        in
        Tensor $ runST $ do
            newArr <- A.new arrayLen
            idx <- newSTRef (0::Int)
            let at = \x -> do
                 i <- readSTRef idx
                 A.write newArr i x
                 writeSTRef idx (i+1)
            mapM_ at lst'
            A.freeze newArr

    toList = undefined

type TensorBoxed (dims::[Nat]) e = Tensor A.ArrayBoxed dims e

type TensorPrim (dims::[Nat]) e = Tensor A.ArrayPrim dims e

type MultiIndex (dims::[Nat]) = SizedList (Length dims) Int

-- * Indexing

type PIndexConstraint a m e (dims::[Nat]) (multiIndex::[Nat]) =
    ( TensorC a m e
    , InBounds dims multiIndex
    , KnownNat (MultiIndexToI dims multiIndex)
    )

pIndex
    :: forall a m e (dims::[Nat]) (multiIndex::[Nat]).
    PIndexConstraint a m e dims multiIndex
    => Tensor a dims e
    -> Proxy multiIndex
    -> e
pIndex (Tensor arr) p = A.index arr (multiIndexToI' (Proxy::Proxy dims) p)

unsafeIndex
    :: forall a m e (dims::[Nat]).
    ( TensorC a m e
    , KnownType dims (MultiIndex dims)
    )
    => Tensor a dims e
    -> MultiIndex dims
    -> e
unsafeIndex (Tensor arr) multiIndex =
    UNSAFE_CHECK
        (inBounds p multiIndex)
        "unsafeIndex"
        ((summon p)::MultiIndex dims, multiIndex)
        $ A.index arr (multiIndexToI p multiIndex)
    where
      p :: Proxy dims
      p = Proxy

index
    :: forall a m e (dims::[Nat]).
    ( TensorC a m e
    , KnownType dims (MultiIndex dims)
    )
    => Tensor a dims e
    -> MultiIndex dims
    -> e
index t multiIndex =
    BOUNDS_CHECK
        (inBounds p multiIndex)
        "index"
        ((summon p)::MultiIndex dims, multiIndex)
        $ unsafeIndex t multiIndex
    where
      p :: Proxy dims
      p = Proxy

maybeIndex
    :: forall a m e (dims::[Nat]).
    ( TensorC a m e
    , KnownType dims (MultiIndex dims)
    )
    => Tensor a dims e
    -> MultiIndex dims
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

type GenerateConstraint a m e (dims::[Nat]) =
    ( TensorC a m e
    , KnownType (AllMultiIndicesInBounds dims) [MultiIndex dims]
    , KnownNat (Product dims)
    )

generate
    :: forall a m e (dims::[Nat]).
    GenerateConstraint a m e dims
    => (MultiIndex dims -> e)
    -> Tensor a dims e
generate f = Tensor $ fromListN len (fmap f lists)
  where
    len :: Int
    len = summon (Proxy::Proxy (Product dims))
    lists :: [MultiIndex dims]
    lists = summon (Proxy::Proxy (AllMultiIndicesInBounds dims))

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

type TensorProductConstraint a m e (ds1::[Nat]) (ds2::[Nat]) =
    ( TensorC a m e
    , Multiplicative e
    , KnownNat (Product ds1)
    , KnownNat (Product ds2)
    )


tensorProduct
    :: forall a m e (ds1::[Nat]) (ds2::[Nat])
    . TensorProductConstraint a m e ds1 ds2
    => Tensor a ds1 e
    -> Tensor a ds2 e
    -> Tensor a (ds1 ++ ds2) e
tensorProduct (Tensor arr1) (Tensor arr2) = Tensor $ runST $ do
    -- this is faster than using unfoldN or generate
    newArr <- A.new (len1 * len2)
    let oneRow = \i ->
         let x = A.index arr1 i
             i' = i*len2
         in
         mapM_
            (\j -> A.write newArr (j+i') (x *. A.index arr2 j))
            [0 .. len2-1]
    mapM_ oneRow [0..len1-1]
    A.freeze newArr
  where
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

type ContractConstraint a m e (dims::[Nat]) (i::Nat) (j::Nat) =
    ( Not (Equal i j) ~ 'True
    , i+1 <= Length dims
    , j+1 <= Length dims
    , (dims !! i) ~ (dims !! j)
    , KnownNat (dims !! i)
    , KnownNat (IJOffset dims i j)
    , KnownNat (Product (ContractedDims dims i j))
    , KnownNat (OthersOffset dims i j)
    , Additive e
    , TensorC a m e
    )

type IJOffset (dims::[Nat]) (i::Nat) (j::Nat) =
    Product (Drop (i+1) dims) + Product (Drop (j+1) dims)

type OthersOffset (dims::[Nat]) (i::Nat) (j::Nat) =
    Sum (Offsets dims) - (IJOffset dims i j)

contract
    :: forall a m e (dims::[Nat]) (i::Nat) (j::Nat)
    . ContractConstraint a m e dims i j
    => Tensor a dims e
    -> Proxy i
    -> Proxy j
    -> Tensor a (ContractedDims dims i j) e
contract (Tensor arr) _ _ = Tensor $ contract_ arr
    (summon (Proxy::Proxy (Product (ContractedDims dims i j))))
    (summon (Proxy::Proxy (dims !! i)))
    (summon (Proxy::Proxy (IJOffset dims i j)))
    (summon (Proxy::Proxy (OthersOffset dims i j)))

type family ContractedDims (xs::[Nat]) (i::Nat) (j::Nat) :: [Nat] where
    ContractedDims xs i j = Delete (Delete xs (Max i j)) (Min i j)

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

dot
    :: (Additive e, Multiplicative e, TensorC a m e)
    => Tensor a '[dim] e
    -> Tensor a '[dim] e
    -> e
dot (Tensor arr1) (Tensor arr2) =
    INTERNAL_CHECK
        (len1 == len2)
        "dot"
        (len1, len2)
        f 1 (g 0)
  where
    f !i !sum
      | i < len1 = f (i+1) (sum +. g i)
      | otherwise = sum
    g i = A.index arr1 i *. A.index arr2 i
    len1 = A.length arr1
    len2 = A.length arr2

instance (Additive e, TensorC a m e) => Additive (Tensor a dims e) where
    {-# INLINE (+.) #-}
    (+.) = add

instance
    ( WithZero e
    , TensorC a m e
    , KnownNat (Product dims)
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
