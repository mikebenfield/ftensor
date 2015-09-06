{-# LANGUAGE UndecidableInstances #-} -- for IsList

module Math.FTensor.Lib.Simple (
    TensorContents(..),
    TensorIsListConstraint,
) where

import Control.Monad.ST
import Data.Proxy
import Data.STRef
import GHC.Exts (IsList(..))
import GHC.TypeLits

import Math.FTensor.Internal.TaggedList
import Math.FTensor.Lib.Array

data TensorContents (dim::Nat) (slotCount::Nat) a e where
    Zero :: !e -> TensorContents dim 0 a e
    Positive :: !a -> TensorContents dim (n+1) a e

type TensorIsListConstraint (dim::Nat) (slotCount::Nat) (scm1::Nat) e =
    ( (scm1+1) ~ slotCount
    , IsList (TaggedList dim (NatToNat_ scm1) e)
    , Item (TaggedList dim (NatToNat_ scm1) e) ~
        IsListContents dim slotCount e
    , KnownNat dim
    , KnownNat slotCount
    )

instance
    ( TensorIsListConstraint dim slotCount scm1 e
    , Array a m
    , Item a ~ e
    )
    => IsList (TensorContents dim slotCount a e) where

    type Item (TensorContents dim slotCount a e) =
        IsListContents dim slotCount e

    fromList lst = fromListN (Prelude.length lst) lst

    fromListN len lst =
        let (dim::Int) = fromInteger . natVal $ (Proxy::Proxy dim)
            (slotCount::Int) = fromInteger . natVal $ (Proxy::Proxy slotCount)
            (lst'::TaggedList dim (NatToNat_ scm1) e) = fromListN len lst
        in
        Positive $ runST $ do
            newArr <- new (dim^slotCount)
            idx <- newSTRef (0::Int)
            let at = \x -> do
                 i <- readSTRef idx
                 write newArr i x
                 writeSTRef idx (i+1)
            mapM_ at lst'
            freeze newArr

    toList = undefined
