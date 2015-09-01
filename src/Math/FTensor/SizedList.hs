{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Math.FTensor.SizedList (
    SizedList(..),
    (++),
    length,
    head,
    tail,
    reverse,
    replicate,
    take,
    drop,
    splitAt,
    sDot,
    unsafeVectorToSizedList,
    sizedListToVector,
    YesNoList(..),
    keepNo,
    keepYes,
    insertAtYes,
    splitYesNo
) where

import Prelude hiding ((++), head, tail, reverse, length,
                       take, drop, replicate, splitAt)

import Data.Proxy
import Unsafe.Coerce
import GHC.TypeLits

import Control.DeepSeq
import qualified Data.Vector.Generic as G

data SizedList (n::Nat) a where
    N :: SizedList 0 a
    (:-) :: a -> SizedList n a -> SizedList (n+1) a
infixr 5 :-

deriving instance Show a => Show (SizedList n a)
deriving instance Eq a => Eq (SizedList n a)

instance Functor (SizedList n) where
    fmap f (x:-xs) = f x :- fmap f xs
    fmap _ N = N

instance NFData a => NFData (SizedList n a) where
    rnf N = ()
    rnf (x:-xs) = rnf (x, xs)

(++) :: SizedList n a -> SizedList m a -> SizedList (n+m) a
(++) N r = r
(++) (x:-xs) r = x :- (++) xs r

infixr 5 ++

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

instance Foldable (SizedList n) where
    foldr _ accum N = accum
    foldr f accum (x:-xs) = foldr f (f x accum) xs

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

-- maybeFromList :: forall n a. KnownNat n => [a] -> Maybe (SizedList n a)
-- maybeFromList []
--   | natVal (Proxy::Proxy n) == 0 = Just (unsafeCoerce N)
--   | otherwise = Nothing
-- maybeFromList (x:xs) = do
--     (xs'::SizedList (n-1) a) <- maybeFromList xs
--     return (x :- unsafeCoerce xs')

sDot :: Num a => SizedList n a -> SizedList n a -> a
sDot N N = 0
sDot (x:-xs) (y:-ys) = x*y + sDot xs ys
sDot _ _ = error "sDot: can't happen"

unsafeVectorToSizedList :: G.Vector v a => v a -> SizedList n a
unsafeVectorToSizedList vec =
    f 0
  where
    len = G.length vec
    f :: Int -> SizedList m a
    f i =
        if i == len
           then unsafeCoerce N
           else unsafeCoerce $ G.unsafeIndex vec i :- f (i+1)

sizedListToVector :: forall v a n. G.Vector v a => SizedList n a -> v a
sizedListToVector list =
    G.unfoldr f list
  where
    f :: SizedList m a -> Maybe (a, SizedList m a)
    f N = Nothing
    f (x:-xs) = Just (x, unsafeCoerce xs)

data YesNoList (size::Nat) (yesCount::Nat) where
    E :: YesNoList 0 0
    No :: YesNoList size yesCount -> YesNoList (size+1) yesCount
    Yes :: YesNoList size yesCount -> YesNoList (size+1) (yesCount+1)

keepNo
    :: SizedList (sizeMinusYeses + yeses') a
    -> YesNoList (sizeMinusYeses + yeses') yeses'
    -> SizedList sizeMinusYeses a
keepNo N E = N
keepNo (_:-xs) (Yes ys) = keepNo xs ys
keepNo (x:-xs) (No ys) =
    unsafeCoerce (x :- keepNo (unsafeCoerce xs) (unsafeCoerce ys))
keepNo _ _ = error "keepNo: can't happen"

keepYes
    :: SizedList (sizeMinusYes + yeses') a
    -> YesNoList (sizeMinusYes + yeses') yeses'
    -> SizedList yeses' a
keepYes N E = N
keepYes (_:-xs) (No ys) = keepYes (unsafeCoerce xs) (unsafeCoerce ys)
keepYes (x:-xs) (Yes ys) =
    unsafeCoerce (x :- keepYes (unsafeCoerce xs) (unsafeCoerce ys))
keepYes _ _ = error "keepYes: can't happen"

insertAtYes
    :: SizedList sizeMinusYes a
    -> YesNoList (sizeMinusYes + yeses') yeses'
    -> a
    -> SizedList (sizeMinusYes + yeses') a
insertAtYes xs E _ = xs
insertAtYes (x:-xs) (No lst) i = x :- insertAtYes xs lst i
insertAtYes xs (Yes lst) i = i :- insertAtYes xs lst i
insertAtYes _ _ _ = error "insertAtYes: can't happen"

splitYesNo
    :: SizedList (sizeMinusYeses + yeses') a
    -> YesNoList (sizeMinusYeses + yeses') yeses'
    -> (SizedList yeses' a, SizedList sizeMinusYeses a)
splitYesNo N E = (N, N)
splitYesNo (x:-xs) (Yes ys) =
    let (first, second) = splitYesNo xs ys
    in
    (x :- first, second)
splitYesNo (x:-xs) (No ys) =
    let (first, second) = splitYesNo (unsafeCoerce xs) (unsafeCoerce ys)
    in
    unsafeCoerce (first, x :- second)
splitYesNo _ _ = error "splitYesNo: can't happen"