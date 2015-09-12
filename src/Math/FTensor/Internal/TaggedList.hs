{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}

module Math.FTensor.Internal.TaggedList (
    TaggedList,
    IsListItem,
) where

import Data.Proxy
import GHC.Exts (IsList(..))
import GHC.TypeLits

import qualified Math.FTensor.Internal.Check

import Math.FTensor.Lib.TypeList

#include "ftensor.h"

data TaggedList (lengths::[Nat]) item where
    One
        :: [item]
        -> TaggedList '[len] item
    More
        :: [TaggedList (len2 ': lens) item]
        -> TaggedList (len1 ': len2 ': lens) item

deriving instance Eq item => Eq (TaggedList lens item)
deriving instance Functor (TaggedList lengths)
deriving instance Foldable (TaggedList lengths)
deriving instance Traversable (TaggedList lengths)

instance KnownNat len => IsList (TaggedList '[len] item) where
    type Item (TaggedList '[len] item) = item

    fromList list = fromListN (Prelude.length list) list

    fromListN length' list =
        BOUNDS_CHECK
            (length' == length)
            "fromListN"
            (length', length)
            $ One list
      where
        length = summon (Proxy::Proxy len)

    toList (One lst) = lst

instance (KnownNat length1, KnownNat length2,
         KnownType lengths [Int]) =>
    IsList (TaggedList (length1 ': length2 ': lengths) item) where

    type Item (TaggedList (length1 ': length2 ': lengths) item) =
        TaggedList (length2 ': lengths) item

    fromList list = fromListN (Prelude.length list) list

    fromListN length' list =
        BOUNDS_CHECK
            (length' == length)
            "fromListN"
            (length', length)
            $ More list
      where
        length = summon (Proxy::Proxy length1)

    toList (More lst) = lst

type family IsListItem (lens::[Nat]) e where
    IsListItem (x ': '[]) e = e
    IsListItem (x ': xs) e = TaggedList xs e
