{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-} -- for NatToNat_

module Math.FTensor.InternalTaggedList (
    Nat_(..),
    Nat_ToNat,
    NatToNat_,
    TaggedList,
    IsListContents,
) where

import Data.Proxy
import Data.Traversable (fmapDefault)
import GHC.Exts (IsList(..))
import GHC.TypeLits

import qualified Math.FTensor.InternalCheck

#include "ftensor.h"

-- * Creating

data Nat_ = Z | S Nat_

type family Nat_ToNat n :: Nat where
    Nat_ToNat 'Z = 0
    Nat_ToNat ('S n) = Nat_ToNat n

type family NatToNat_ n :: Nat_ where
    NatToNat_ 0 = 'Z
    NatToNat_ n = 'S (NatToNat_ (n-1))

data TaggedList (length::Nat) (depth::Nat_) item where
    One :: [item] -> TaggedList length 'Z item
    More :: [TaggedList length depth item] -> TaggedList length ('S depth) item

-- this instance is kind of a fib
instance Show item => Show (TaggedList length depth item) where
    show (One lst) = show lst
    show (More lst) = show lst

instance KnownNat length => IsList (TaggedList length 'Z item) where
    type Item (TaggedList length 'Z item) = item

    fromList lst = fromListN (Prelude.length lst) lst

    fromListN length'
      | length' == length = One
      | otherwise = error "fromListN: list wrong length"
      where
        length = fromInteger . natVal $ (Proxy::Proxy length)

    toList (One lst) = lst

instance KnownNat length => IsList (TaggedList length ('S d) item) where
    type Item (TaggedList length ('S d) item) = TaggedList length d item

    fromList lst = fromListN (Prelude.length lst) lst

    fromListN length' lst =
        BOUNDS_CHECK
            (length' == length)
            "fromListN"
            (length', length)
            More lst
      where
        length = fromInteger . natVal $ (Proxy::Proxy length)

    toList (More lst) = lst

instance Foldable (TaggedList length depth) where
    foldr f x (One lst) = foldr f x lst
    foldr f x (More lst) = foldr (flip $ foldr f) x lst

instance Functor (TaggedList length depth) where
    fmap = fmapDefault

instance Traversable (TaggedList length depth) where
    traverse f (One lst) = pure One <*> traverse f lst
    traverse f (More lst) = pure More <*> traverse (traverse f) lst

type family IsListContents (length::Nat) (depth::Nat) e where
    IsListContents length 1 e = e
    IsListContents length n e = TaggedList length (NatToNat_ (n-2)) e
