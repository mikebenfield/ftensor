{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-} -- for IsList

module Math.FTensor.Lib.General (
    Tensor(..),
    TensorC,
    MultiIndexToI,
    multiIndexToI',
    multiIndexToI,
    InBounds,
    inBounds,
    AllMultiIndicesInBounds,
) where

import Control.Monad.ST
import Data.Proxy
import Data.STRef
import GHC.Exts (IsList(..), Constraint)
import GHC.TypeLits

import Control.DeepSeq

import Math.FTensor.Internal.TaggedList
import Math.FTensor.Lib.Array
import Math.FTensor.SizedList
import Math.FTensor.Lib.TypeList

newtype Tensor a (dims::[Nat]) e = Tensor (a e)
  deriving (Eq, Show, Functor, Traversable, Foldable)

type TensorC a m e = (Array (a e) m, Item (a e) ~ e)

--deriving instance Show (a e) => Show (Tensor a dims e) -- XXX temporary

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

type family InBounds (dims::[Nat]) (multiIndex::[Nat]) :: Constraint where
    InBounds '[] '[] = ()
    InBounds (d ': ds) (i ': is) = (i+1 <= d, InBounds ds is)

inBounds
    :: forall (dims::[Nat])
    . KnownType dims (SizedList (Length dims) Int)
    => Proxy (dims::[Nat])
    -> SizedList (Length dims) Int
    -> Bool
inBounds p = f ((summon p) :: SizedList (Length dims) Int)
  where
    f :: SizedList len Int -> SizedList len Int -> Bool
    f (dim :- dims) (i :- is) = 0 <= i && i < dim && f dims is
    f _ _ = True

type family MultiIndexToI (dims::[Nat]) (multiIndex::[Nat]) :: Nat where
    MultiIndexToI '[] '[] = 0
    MultiIndexToI (dim ': dims) is = MultiIndexToI_ dims is 0

type family MultiIndexToI_ (dims::[Nat]) (multiIndex::[Nat]) (accum::Nat)
    :: Nat where

    MultiIndexToI_ '[] '[i] accum = accum + i
    MultiIndexToI_ (dim ': dims) (i ': is) accum =
        MultiIndexToI_ dims is (dim*(accum + i))

multiIndexToI'
    :: forall (dims::[Nat]) (multiIndex::[Nat]) (result::Nat)
    . (KnownNat result, result ~ MultiIndexToI dims multiIndex)
    => Proxy dims
    -> Proxy multiIndex
    -> Int
multiIndexToI' _ _ = natIntVal (Proxy::Proxy result)

multiIndexToI
    :: forall (dims::[Nat])
    . KnownType dims (SizedList (Length dims) Int)
    => Proxy dims
    -> SizedList (Length dims) Int
    -> Int
multiIndexToI p multiIndex =
    case (summon p) :: SizedList (Length dims) Int of
        N -> 0
        _ :- dims -> f dims multiIndex 0
  where
    f :: SizedList n Int -> SizedList (n+1) Int -> Int -> Int
    f N (i :- N) accum = accum + i
    f (dim :- dims) (i :- is) accum = f dims is (dim*(accum + i))
    f _ _ _ = error "multiIndexToI: can't happen"

type family AllMultiIndicesInBounds (dims::[Nat]) :: [[Nat]] where
    AllMultiIndicesInBounds '[] = '[ '[]]
    AllMultiIndicesInBounds (d ': ds) =
        CartesianProduct (EnumFromTo 0 (d-1)) (AllMultiIndicesInBounds ds)
