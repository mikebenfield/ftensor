{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-} -- for NatToNat_

module Math.FTensor.Internal.TaggedList (
    TaggedList,
    IsListItem,
) where

import Data.Proxy
import Data.Traversable (fmapDefault)
import GHC.Exts (IsList(..))
import GHC.TypeLits

import Math.FTensor.Lib.TypeList
import qualified Math.FTensor.Internal.Check

#include "ftensor.h"

data TaggedList (lengths::[Nat]) item where
    One
        :: [item]
        -> TaggedList '[len] item
    More
        :: [TaggedList (len2 ': lens) item]
        -> TaggedList (len1 ': len2 ': lens) item

deriving instance Eq item => Eq (TaggedList lens item)

instance Foldable (TaggedList lens) where
    foldr f x (One lst) = foldr f x lst
    foldr f x (More lst) = foldr (flip $ foldr f) x lst

instance Functor (TaggedList lens) where
    fmap = fmapDefault

instance Traversable (TaggedList lens) where
    traverse f (One lst) = pure One <*> traverse f lst
    traverse f (More lst) = pure More <*> traverse (traverse f) lst

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
        length = natIntVal (Proxy::Proxy len)

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
        length = natIntVal (Proxy::Proxy length1)

    toList (More lst) = lst

type family IsListItem (lens::[Nat]) e where
    IsListItem (x ': '[]) e = e
    IsListItem (x ': xs) e = TaggedList xs e
