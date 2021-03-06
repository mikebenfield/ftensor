{-|
Module: Math.Ftensor.Lib.TypeList
Copyright: (c) 2015 Michael Benfield
License: ISC

Functions for working with type level lists.

Like other modules under @Math.Ftensor.Lib@, casual users of the library should
not need to directly use this module.
-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Math.Ftensor.Lib.TypeList (
    -- * Tuples
    Fst,
    Snd,

    -- * Misc
    If,
    Min,
    Max,
    And,
    Or,
    Not,
    Equal,

    -- * List operations
    type (!!),
    type (++),
    Concat,
    Length,
    Delete,
    Head,
    Tail,
    Reverse,
    Replicate,
    Take,
    Drop,
    SplitAt,
    EnumFromTo,
    AttachEach,
    CartesianProduct,
    Sum,
    Product,

    -- * Types to values
    KnownType(..),
) where

import Prelude hiding ((++))

import Data.Proxy
import GHC.TypeLits

import Math.Ftensor.SizedList hiding ((++))

-- * Tuples

type family Fst (x :: (k1, k2)) :: k1 where
    Fst ('(,) a b) = a

type family Snd (x :: (k1, k2)) :: k1 where
    Snd ('(,) a b) = b

-- * Misc

type family If (b::Bool) (x::k) (y::k) :: k where
    If 'True x y = x
    If 'False x y = y

type family Min (x::Nat) (y::Nat) :: Nat where
    Min x y = If (x <=? y) x y

type family Max (x::Nat) (y::Nat) :: Nat where
    Max x y = If (x <=? y) y x

type family And (x::Bool) (y::Bool) :: Bool where
    And 'True 'True = 'True
    And x y = 'False

type family Not (x::Bool) :: Bool where
    Not 'True = 'False
    Not 'False = 'True

type family Or (x::Bool) (y::Bool) :: Bool where
    Or 'False 'False = 'False
    Or x y = 'True

type Equal (m::Nat) (n::Nat) = And (m <=? n) (n <=? m)

-- * List operations

type family (!!) (list::[k]) (n::Nat) :: k where
    (!!) (x ': xs) 0 = x
    (!!) (x ': xs) n = (!!) xs (n-1)

type family (++) (lhs::[k]) (rhs::[k]) :: [k] where
    (++) '[] rhs = rhs
    (++) (x ': xs) rhs = x ': ((++) xs rhs)

type family Concat (args::[[k]]) :: [k] where
    Concat '[] = '[]
    Concat (x ': xs) = x ++ Concat xs

type family Length (x::[k]) :: Nat where
    Length '[] = 0
    Length (x ': xs) = 1 + Length xs

type family Delete (xs::[k]) (n::Nat) :: [k] where
    Delete (x ': xs) 0 = xs
    Delete (x ': xs) n = x ': Delete xs (n-1)

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

type family EnumFromTo (from::Nat) (to::Nat) :: [Nat] where
    EnumFromTo a a = '[a]
    EnumFromTo a b = a ': EnumFromTo (a+1) b

type family AttachEach (a::k) (bs::[[k]]) :: [[k]] where
    AttachEach a '[] = '[]
    AttachEach a (b ': bs) = (a ': b) ': AttachEach a bs

type family CartesianProduct (a::[k]) (bs::[[k]]) :: [[k]] where
    CartesianProduct '[] bs = '[]
    CartesianProduct (a ': as) bs =
        (AttachEach a bs) ++ CartesianProduct as bs

type family Sum (ns::[Nat]) :: Nat where
    Sum '[] = 0
    Sum (n ': ns) = n + Sum ns

type family Product (ns::[Nat]) :: Nat where
    Product '[] = 1
    Product (n ': ns) = n * Product ns

-- * Types to values

class KnownType (typ::k) result where
    summon :: (Proxy typ) -> result

instance KnownNat n => KnownType n Integer where
    {-# INLINE summon #-}
    summon = natVal

instance KnownNat n => KnownType n Int where
    {-# INLINE summon #-}
    summon = fromInteger . natVal

instance KnownType '[] [a] where
    {-# INLINE summon #-}
    summon _ = []

instance (KnownType d a, KnownType ds [a]) => KnownType (d ': ds) [a] where
    summon _ =
        summon (Proxy::Proxy d) : summon (Proxy::Proxy ds)

instance KnownType '[] (SizedList 0 a) where
    {-# INLINE summon #-}
    summon _ = N

instance
    ( len ~ (Length ds + 1)
    , len ~ (1 + Length ds)
    , KnownType d a
    , KnownType ds (SizedList (Length ds) a)
    )
    => KnownType (d ': ds) (SizedList len a) where
    summon _ = summon (Proxy::Proxy d) :- summon (Proxy::Proxy ds)

instance KnownType 'True Bool where
    summon _ = True

instance KnownType 'False Bool where
    summon _ = False
