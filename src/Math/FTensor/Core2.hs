{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Math.FTensor.Core2 (
    Tensor,
    TensorV,
    TensorU,

    generate,
    shape,
    convert,
    unsafeIndex,
    index,
    maybeIndex,
    scalar,
    tensor,

    unsafeAdd,
    add,
    tensorProduct,
    unsafeContract,
    contract,
    -- changeBasis,
) where

import Data.Maybe (fromMaybe)
import Data.Proxy
import GHC.Exts
import GHC.TypeLits
import Unsafe.Coerce

import Control.DeepSeq
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

import Math.FTensor.Internal
import Math.FTensor.SizedList (SizedList(..))

#include "ftensor.h"

data Tensor v a (n::Nat) = Tensor !(v a) {-# UNPACK #-} !Offsets

type TensorV = Tensor V.Vector

type TensorU = Tensor U.Vector

instance (Eq a, G.Vector v a) => Eq (Tensor v a n) where
    Tensor vec1 offsets1 == Tensor vec2 offsets2 =
        offsets1 == offsets2 && G.eq vec1 vec2

instance (IsListConstraint v a n, Show (NestedList a n))
    => Show (Tensor v a n) where
    show = ("fromList " ++) . show . toList

instance {-# OVERLAPPING #-} (Show a, G.Vector v a) => Show (Tensor v a 0)
  where
    show (Tensor vec _) = "tensor " ++ show (vec G.! 0)

instance (NFData a, NFData (v a)) => NFData (Tensor v a n) where
    rnf (Tensor v o) = rnf (v, o)

-- instance (Functor v) => Functor (Tensor v a n) where
--     fmap f (Tensor vec shape) = Tensor (fmap f vec) shape

-- bounds checking
check :: Show b => String -> Int -> String -> Bool -> String -> b -> a -> a
check checkType lineNumber fileName False functionName otherData _ =
    error $ fileName ++ " (" ++ (show lineNumber) ++ ") (" ++
        functionName ++ "): Failed " ++ checkType ++ "check: " ++
            (show otherData)
check _ _ _ _ _ _ x = x

noCheck :: a -> b -> c -> d -> d
noCheck _ _ _ x = x
{-# INLINE noCheck #-}

generate :: G.Vector v a => Shape n -> (MultiIndex n -> a) -> Tensor v a n
generate shape f =
    Tensor (G.generate length (f . unsafeIndexToMultiIndex offsets)) offsets
  where
    (length, offsets) = shapeToOffsets shape

shape :: G.Vector v a => Tensor v a n -> Shape n
shape (Tensor vec offsets) = unsafeOffsetsToShape (G.length vec) offsets

convert :: (G.Vector v a, G.Vector w a) => Tensor v a n -> Tensor w a n
convert (Tensor vec offsets) = Tensor (G.convert vec) offsets

unsafeIndex :: (G.Vector v a) => Tensor v a n -> MultiIndex n -> a
unsafeIndex (Tensor vec offsets) i =
    UNSAFE_CHECK
        (inBoundsOffsets (G.length vec) offsets i)
        "unsafeIndex"
        (offsets, i)
        $ G.unsafeIndex vec (multiIndexToIndex offsets i)

index :: (G.Vector v a) => Tensor v a n -> MultiIndex n -> a
index t@(Tensor vec offsets) i =
    BOUNDS_CHECK
        (inBoundsOffsets (G.length vec) offsets i)
        "index"
        (offsets, i)
        $ unsafeIndex t i

maybeIndex :: (G.Vector v a) => Tensor v a n -> MultiIndex n -> Maybe a
maybeIndex t@(Tensor vec offsets) i
  | inBoundsOffsets (G.length vec) offsets i = Just $ unsafeIndex t i
  | otherwise = Nothing

scalar :: G.Vector v a => Tensor v a 0 -> a
scalar (Tensor vec offsets) =
    INTERNAL_CHECK
        (G.length vec == 1)
        "scalar"
        (G.length vec, offsets)
        $ G.unsafeIndex vec 0

tensor :: G.Vector v a => a -> Tensor v a 0
tensor x = Tensor (G.cons x G.empty) G.empty

unsafeAdd :: (Num a, G.Vector v a) => Tensor v a n -> Tensor v a n -> Tensor v a n
unsafeAdd (Tensor v1 offsets1) (Tensor v2 offsets2) =
    UNSAFE_CHECK
        (offsets1 == offsets2 && G.length v1 == G.length v2)
        "unsafeAdd"
        (offsets1, offsets2)
        $ Tensor (G.zipWith (+) v1 v2) offsets1

add :: (Num a, G.Vector v a) => Tensor v a n -> Tensor v a n -> Tensor v a n
add t1@(Tensor v1 offsets1) t2@(Tensor v2 offsets2) =
    UNSAFE_CHECK
        (offsets1 == offsets2 && G.length v1 == G.length v2)
        "add"
        (offsets1, offsets2)
        $ unsafeAdd t1 t2

tensorProduct
    :: forall v a (m::Nat) (n::Nat). (Num a, G.Vector v a)
    => Tensor v a n
    -> Tensor v a m
    -> Tensor v a (n+m)
tensorProduct (Tensor v1 offsets1) (Tensor v2 offsets2) =
    Tensor newVec newOffsets
  where
    newVec = G.concat $ map (\x -> G.map (*x) v2) (G.toList v1)
    newOffsets = G.map (* G.length v2) offsets1 G.++ offsets2
{-# INLINE[1] tensorProduct #-}

-- {-# RULES
--     "tensorProduct/scalarScalar" tensorProduct = tensorProductScalarScalar
--     "tensorProduct/scalar" tensorProduct = tensorProductScalar
-- #-}

-- tensorProductScalar
--     :: forall v a (m::Nat). (Num a, G.Vector v a)
--     => Tensor v a 0
--     -> Tensor v a m
--     -> Tensor v a m
-- tensorProductScalar (Tensor v1 _) (Tensor v2 shape) = Tensor newVec shape
--   where
--     newVec = G.map (* (G.unsafeIndex v1 0)) v2
-- {-# INLINE tensorProductScalar #-}

-- tensorProductScalarScalar
--     :: forall v a. (Num a, G.Vector v a)
--     => Tensor v a 0
--     -> Tensor v a 0
--     -> Tensor v a 0
-- tensorProductScalarScalar (Tensor v1 _) (Tensor v2 _) = Tensor newVec N
--   where
--     newVec = G.cons (G.unsafeIndex v1 0 * G.unsafeIndex v2 0) G.empty
-- {-# INLINE tensorProductScalarScalar #-}

unsafeContract
    :: forall v a (n::Nat). (Num a, G.Vector v a)
    => Tensor v a (n+2)
    -> Int
    -> Int
    -> Tensor v a n
unsafeContract (Tensor vec offsets) i j =
    UNSAFE_CHECK
        (0 <= i && i < j && j < G.length offsets && iSize == jSize)
        "unsafeContract"
        (offsets, i, j)
        (Tensor newVec newOffsets)
  where
    shapeVec = offsetsToShapeVector (G.length vec) offsets
    newShapeVec =
        let (beforeJ, afterJ) = G.splitAt j shapeVec
            (beforeI, middle) = G.splitAt i beforeJ
        in
        beforeI G.++ (G.tail middle) G.++ (G.tail afterJ)
    (newLength, newOffsets) = shapeVectorToOffsets newShapeVec
    iSize = shapeVec G.! i
    jSize = shapeVec G.! j
    iOffset = offsets G.! i
    jOffset = offsets G.! j
    sumOffset = iOffset + jOffset
    othersOffset = G.sum offsets - sumOffset
    sumStartingAt i =
        let vec' :: v a
            vec' = G.generate iSize (\k -> vec G.! (i+sumOffset*k))
        in
        G.sum vec'
    newVec = G.generate newLength (sumStartingAt . (*othersOffset))

contract
    :: forall v a (n::Nat). (Num a, G.Vector v a)
    => Tensor v a (n+2)
    -> Int
    -> Int
    -> Tensor v a n
contract t@(Tensor vec offsets) i j =
    BOUNDS_CHECK
        (0 <= i && i < j && j < G.length offsets && iSize == jSize)
        "unsafeContract"
        (offsets, i, j)
        $ unsafeContract t i j
  where
    shapeVec = offsetsToShapeVector (G.length vec) offsets
    iSize = shapeVec G.! i
    jSize = shapeVec G.! j

-- changeBasis
--     :: (Num a, G.Vector v a)
--     => Tensor v a n
--     -> YesNoList n m
--     -> Tensor v a 2
--     -> Tensor v a n
-- changeBasis = undefined

type family NestedList a (n::Nat) where
    NestedList a 0 = a
    NestedList a n = [NestedList a (n-1)]

class Flattenable x a (n::Nat) where
    flatten :: Proxy n -> x -> [a]

    listShape :: Proxy a -> x -> Shape n

    shapeList :: Shape n -> [a] -> x

nLists :: Int -> [a] -> [[a]]
nLists n lst = case Prelude.splitAt n lst of
    ([], _) -> []
    (firstN, rest) -> firstN : nLists n rest

instance Flattenable [x] x 1 where

    flatten _ = id

    listShape _ lst = Prelude.length lst :- N

    shapeList (i:-N) lst = lst
    shapeList _ _ = error "shapeList: can't happen"

instance forall x y (n::Nat).
    Flattenable [x] y (n-1) => Flattenable [[x]] y n where

    flatten _ xxs = concatMap (flatten (Proxy::Proxy (n-1))) xxs

    listShape _ lst@(hd:_) =
        let sh::Shape (n-1) = listShape (Proxy::Proxy y) hd
        in
        Prelude.length lst :- sh
    listShape _ _ = error "listShape"

    shapeList (i :- (a@(j:-_)::Shape (n-1))) lst =
        nLists j $ shapeList a lst
    shapeList _ _ = error "shapeList: can't happen"

type IsListConstraint v a (n::Nat) =
    (G.Vector v a, Flattenable (NestedList a n) a n,
    NestedList a n ~ [NestedList a (n-1)])

instance forall v a (n::Nat). IsListConstraint v a n
    => IsList (Tensor v a n) where

    type Item (Tensor v a n) = NestedList a (n-1)

    fromList lst =
        let shape :: Shape n
            shape = listShape (Proxy::Proxy a) lst
        in
        Tensor
            (G.fromList $ flatten (Proxy::Proxy n) lst)
            (snd $ shapeToOffsets shape)

    toList (Tensor vec offsets) =
        let shape :: Shape n
            shape = unsafeOffsetsToShape (G.length vec) offsets
        in
        shapeList shape (G.toList vec)
