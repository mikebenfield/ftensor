{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Math.FTensor.Lib.TypeList (
    -- * Tuples
    Fst,
    Snd,

    -- * List operations
    (++),
    Concat,
    Length,
    Head,
    Tail,
    Reverse,
    Replicate,
    Take,
    Drop,
    SplitAt,
    Sum,
    Product,

    -- * Types to values
    natIntVal,
    KnownList(..),
) where

import Data.Proxy
import GHC.TypeLits

-- * Tuples

type family Fst (x :: (k1, k2)) :: k1 where
    Fst ('(,) a b) = a

type family Snd (x :: (k1, k2)) :: k1 where
    Snd ('(,) a b) = b

-- * List operations

type family (++) (lhs::[k]) (rhs::[k]) :: [k] where
    (++) '[] rhs = rhs
    (++) (x ': xs) rhs = x ': ((++) xs rhs)

type family Concat (args::[[k]]) :: [k] where
    Concat '[] = '[]
    Concat (x ': xs) = x ++ Concat xs

type family Length (x::[k]) :: Nat where
    Length '[] = 0
    Length (x ': xs) = 1 + Length xs

type family Head (x::[k]) :: k where
    Head (x ': xs) = x

type family Tail (x::[k]) :: [k] where
    Tail (x ': xs) = xs

type family Reverse (x::[k]) :: [k] where
    Reverse '[] = '[]
    Reverse (x ': xs) = Reverse xs ++ (x ': '[])

type family Replicate (n::Nat) (x::k) :: [k] where
    Replicate 0 x = '[]
    Replicate n x = x ': Replicate (n-1) x

type family Take (n::Nat) (list::[k]) :: [k] where
    Take 0 list = '[]
    Take n '[] = '[]
    Take n (x ': xs) = x ': Take (n-1) xs

type family Drop (n::Nat) (list::[k]) :: [k] where
    Drop 0 list = list
    Drop n '[] = '[]
    Drop n (x ': xs) = Drop (n-1) xs

type family SplitAt (n::Nat) (list::[k]) :: ([k], [k]) where
    SplitAt n list = '(,) (Take n list) (Drop n list)

type family Sum (ns::[Nat]) :: Nat where
    Sum '[] = 0
    Sum (n ': ns) = n + Sum ns

type family Product (ns::[Nat]) :: Nat where
    Product '[] = 1
    Product (n ': ns) = n * Product ns

-- * Types to values

{-# INLINE natIntVal #-}
natIntVal :: forall (n::Nat). KnownNat n => Proxy n -> Int
natIntVal = fromInteger . natVal

class KnownList (list::[Nat]) where
    listVal :: Proxy list -> [Integer]
    listIntVal :: Proxy list -> [Int]

instance KnownList ('[]::[Nat]) where
    listVal _ = []
    listIntVal _ = []

instance (KnownList xs, KnownNat x) => KnownList (x ': xs) where
    listVal _ = natVal (Proxy::Proxy x) : listVal (Proxy::Proxy xs)
    listIntVal _ = natIntVal (Proxy::Proxy x) : listIntVal (Proxy::Proxy xs)
