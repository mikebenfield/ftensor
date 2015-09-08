{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-} -- for IsList

module Math.FTensor.Lib.General (
    TensorContents(..),
    TensorIsListConstraint,
) where

import Control.Monad.ST
import Data.Proxy
import Data.STRef
import GHC.Exts (IsList(..), Constraint)
import GHC.TypeLits

import Math.FTensor.Internal.TaggedList
import Math.FTensor.Lib.Array
import Math.FTensor.Lib.TypeList

data TensorContents (dims::[Nat]) a e where
    Zero :: !e -> TensorContents '[] a e
    Positive :: !a -> TensorContents (n ': ns) a e

type TensorIsListConstraint (d::Nat) (ds::[Nat]) e =
    ( IsList (TaggedList2 (d ': ds) e)
    , Item (TaggedList2 (d ': ds) e) ~ IsListItem (d ': ds) e
    , KnownNat (Product (d ': ds))
    )

instance
    forall d ds dims e a m.
    ( dims ~ (d ': ds)
    , TensorIsListConstraint d ds e
    , Array a m
    , Item a ~ e
    )
    => IsList (TensorContents dims a e) where

    type Item (TensorContents dims a e) =
        IsListItem dims e

    fromList lst = fromListN (Prelude.length lst) lst

    fromListN len lst =
        let arrayLen = natIntVal (Proxy::Proxy (Product dims))
            (lst'::TaggedList2 dims e) = fromListN len lst
        in
        Positive $ runST $ do
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
    InBounds (d ': ds) (i ': is) = (i <= d, InBounds ds is)

-- type family MultiIndexToI (dims::[Nat]) (multiIndex::[Nat]) :: Nat where
--     MultiIndexToI (dim ': dims) multiIndex = MultiIndexToI_ dims multiIndex

-- type family MultiIndexToI (dims::[Nat]) (multiIndex::[Nat]) :: Nat where
--     MultiIndexToI (dim ': dims) multiIndex = MultiIndexToI_ dims multiIndex

