{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-} -- for NatToNat_

module Math.FTensor.Internal.TaggedList (
    Nat_(..),
    Nat_ToNat,
    NatToNat_,
    TaggedList,
    IsListContents,
    TaggedList2,
    IsListItem,
) where

import Data.Proxy
import Data.Traversable (fmapDefault)
import GHC.Exts (IsList(..))
import GHC.TypeLits

import Math.FTensor.Lib.TypeList
import qualified Math.FTensor.Internal.Check

#include "ftensor.h"

data TaggedList2 (lengths::[Nat]) item where
    One2
        :: [item]
        -> TaggedList2 '[len] item
    More2
        :: [TaggedList2 (len2 ': lens) item]
        -> TaggedList2 (len1 ': len2 ': lens) item

deriving instance Eq item => Eq (TaggedList2 lens item)

instance Foldable (TaggedList2 lens) where
    foldr f x (One2 lst) = foldr f x lst
    foldr f x (More2 lst) = foldr (flip $ foldr f) x lst

instance Functor (TaggedList2 lens) where
    fmap = fmapDefault

instance Traversable (TaggedList2 lens) where
    traverse f (One2 lst) = pure One2 <*> traverse f lst
    traverse f (More2 lst) = pure More2 <*> traverse (traverse f) lst

instance KnownNat len => IsList (TaggedList2 '[len] item) where
    type Item (TaggedList2 '[len] item) = item

    fromList list = fromListN (Prelude.length list) list

    fromListN length' list =
        BOUNDS_CHECK
            (length' == length)
            "fromListN"
            (length', length)
            $ One2 list
      where
        length = natIntVal (Proxy::Proxy len)

    toList (One2 lst) = lst

instance (KnownNat length1, KnownNat length2,
         KnownType lengths [Int]) =>
    IsList (TaggedList2 (length1 ': length2 ': lengths) item) where

    type Item (TaggedList2 (length1 ': length2 ': lengths) item) =
        TaggedList2 (length2 ': lengths) item

    fromList list = fromListN (Prelude.length list) list

    fromListN length' list =
        BOUNDS_CHECK
            (length' == length)
            "fromListN"
            (length', length)
            $ More2 list
      where
        length = natIntVal (Proxy::Proxy length1)

    toList (More2 lst) = lst

type family IsListItem (lens::[Nat]) e where
    IsListItem (x ': '[]) e = e
    IsListItem (x ': xs) e = TaggedList2 xs e

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
