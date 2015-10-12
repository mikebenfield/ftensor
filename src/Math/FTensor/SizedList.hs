{-|
Module: Math.FTensor.SizedList
Copyright: (c) 2015 Michael Benfield
License: BSD-3

Lists carrying their size in their type. These are used for runtime indexing
into tensors. Many of the functions duplicate functionality for lists from
Haskell's @Prelude@.
-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-} -- for Flattenable

module Math.FTensor.SizedList (
    SizedList(..),
    (++),
    concat,
    length,
    head,
    tail,
    reverse,
    replicate,
    take,
    drop,
    splitAt,
    sDot,
    toList',
) where

import Prelude hiding ((++), head, tail, reverse, length,
                       take, drop, replicate, splitAt, concat)
import qualified Prelude

import Data.Proxy
import Unsafe.Coerce
import GHC.Exts (IsList(..))
import GHC.TypeLits

import Control.DeepSeq

data SizedList (n::Nat) a where
    N :: SizedList 0 a
    (:-) :: a -> SizedList n a -> SizedList (n+1) a
infixr 5 :-

deriving instance Show a => Show (SizedList n a)
deriving instance Eq a => Eq (SizedList n a)
deriving instance Functor (SizedList n)
deriving instance Foldable (SizedList n)
deriving instance Traversable (SizedList n)

instance NFData a => NFData (SizedList n a) where
    rnf N = ()
    rnf (x:-xs) = rnf (x, xs)

(++) :: SizedList n a -> SizedList m a -> SizedList (n+m) a
(++) N r = r
(++) (x:-xs) r = x :- (++) xs r

infixr 5 ++

concat :: SizedList n (SizedList m a) -> SizedList (m*n) a
concat N = N
concat (x:-xs) = x ++ concat xs

length :: SizedList n a -> Int
length N = 0
length (_:-xs) = 1 + length xs

head :: SizedList (n+1) a -> a
head (x:-_) = x
head _ = error "head"

tail :: SizedList (n+1) a -> SizedList n a
tail (_:-xs) = xs
tail _ = error "tail"

reverse :: SizedList n a -> SizedList n a
reverse list = rev list N
  where
    rev :: SizedList p a -> SizedList m a -> SizedList (m+p) a
    rev N ys = ys
    rev (x:-xs) ys = rev xs (x:-ys)

replicate :: forall a n. KnownNat n => a -> SizedList n a
replicate val = unsafeRep (fromInteger $ natVal (Proxy :: Proxy n)) N
  where
    unsafeRep :: Int -> SizedList m a -> SizedList p a
    unsafeRep 0 sl = unsafeCoerce sl
    unsafeRep i sl = unsafeRep (i-1) (val:-sl)

take
    :: KnownNat n
    => Proxy (n::Nat)
    -> SizedList (m+n) a
    -> SizedList n a
take p = fst . splitAt p

drop
    :: KnownNat n
    => Proxy (n::Nat)
    -> SizedList (m+n) a
    -> SizedList m a
drop p = snd . splitAt p

splitAt
    :: KnownNat n
    => Proxy (n::Nat)
    -> SizedList (m+n) a
    -> (SizedList n a, SizedList m a)
splitAt nP list = unsafeSplitAt (fromIntegral $ natVal nP) list
  where
    unsafeSplitAt :: Int -> SizedList p a -> (SizedList q a, SizedList r a)
    unsafeSplitAt 0 list = unsafeCoerce (N, list)
    unsafeSplitAt i list =
        let (first, second) = unsafeSplitAt (i-1) (tail $ unsafeCoerce list)
        in
        unsafeCoerce (head (unsafeCoerce list) :- first, second)

-- | Dot product.
sDot :: Num a => SizedList n a -> SizedList n a -> a
sDot N N = 0
sDot (x:-xs) (y:-ys) = x*y + sDot xs ys
sDot _ _ = error "sDot: can't happen"

fromList_ :: [a] -> (Int, SizedList n a)
fromList_ = f 0
  where
    f i [] = (i, unsafeCoerce N)
    f i (x:xs) =
        let (sum, lst) = f (i+1) xs
        in
        unsafeCoerce (sum, x :- unsafeCoerce lst)

fromList__ :: [a] -> SizedList n a
fromList__ [] = unsafeCoerce N
fromList__ (x:xs) = unsafeCoerce $ x :- fromList__ xs

instance KnownNat n => IsList (SizedList n a) where
    type Item (SizedList n a) = a

    fromList lst
      | len' == len = result
      | otherwise = error $
            "fromList (SizedList): list wrong size" Prelude.++ show (len, len')
      where
        len' = fromInteger . natVal $ (Proxy::Proxy n)
        (len, result) = fromList_ lst

    fromListN len lst
      | len' == len = result
      | otherwise = error $
            "fromListN (SizedList): list wrong size" Prelude.++ show (len, len')
      where
        len' = fromInteger . natVal $ (Proxy::Proxy n)
        result = fromList__ lst

    toList = toList'

-- | Since this function can be called without requiring @KnownNat n@,
-- an implementation separate from the @IsList@ class is provided.
toList' :: SizedList n a -> [a]
toList' N = []
toList' (x:-xs) = x : toList' xs
