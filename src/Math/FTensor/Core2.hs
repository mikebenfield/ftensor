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

    validShape,
    inBounds,
    formatShape,
    unformatShape,
    indexToI,
    iToIndex,

    unsafeGenerate,
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
    changeBasis,
) where
import Debug.Trace -- XXX
import Data.Maybe (fromMaybe)
import Data.Proxy
import GHC.Exts
import GHC.TypeLits
import Unsafe.Coerce

import Control.DeepSeq
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

import Math.FTensor.SizedList hiding ((++))
import qualified Math.FTensor.SizedList as SizedList

#include "ftensor.h"

data Tensor v a (n::Nat) = Tensor (v a) (FormattedShape n)

type TensorV = Tensor V.Vector

type TensorU = Tensor U.Vector

instance (Eq a, G.Vector v a) => Eq (Tensor v a n) where
    Tensor vec1 shape1 == Tensor vec2 shape2 =
        shape1 == shape2 && G.eq vec1 vec2

instance (IsListConstraint v a n, Show (NestedList a n))
    => Show (Tensor v a n) where

    show = ("fromList " ++) . show . toList

instance {-# OVERLAPPING #-} (Show a, G.Vector v a) => Show (Tensor v a 0)
  where
    show (Tensor vec _) = "tensor " ++ show (vec G.! 0)

instance (NFData a, NFData (v a)) => NFData (Tensor v a n) where
    rnf (Tensor v s) = rnf (v, s)

-- instance (Functor v) => Functor (Tensor v a n) where
--     fmap f (Tensor vec shape) = Tensor (fmap f vec) shape

type Shape (n::Nat) = SizedList n Int

type FormattedShape (n::Nat) = SizedList n Int

type Index (n::Nat) = SizedList n Int

-- bounds checking
check :: Show b => Bool -> String -> String -> Int -> String -> b -> a -> a
check False checkType functionName lineNumber fileName otherData _ =
    error $ fileName ++ " (" ++ (show lineNumber) ++ "): Failed " ++
        checkType ++ " check in function " ++ functionName ++
            " with data " ++ (show otherData)
check _ _ _ _ _ _ val = val

validShape :: Shape n -> Bool
validShape N = True
validShape (x:-xs) = x > 0 && validShape xs

inBounds :: Shape n -> Index n -> Bool
inBounds (b:-bs) (i:-is) = i >=0 && i < b && inBounds bs is
inBounds _ _ = True

inBoundsFormatted :: FormattedShape n -> Index n -> Bool
inBoundsFormatted N _ = True
inBoundsFormatted s@(hd:-_) index = 0 <= i && i < hd
  where
     i = indexToI s index

formatShape :: Shape n -> FormattedShape n
formatShape N = N
formatShape s@(_:-N) = s
formatShape (x:-xs) = case formatted of
    hd:-rest -> x*hd :- formatted
    _ -> error "formatShape: can't happen"
  where
    formatted = formatShape xs

unformatShape :: FormattedShape n -> Shape n
unformatShape N = N
unformatShape s@(x:-N) = s
unformatShape (x:-s@(y:-_)) = div x y :- unformatShape s

indexToI :: FormattedShape n -> Index n -> Int
indexToI _ (i:-N) = i
indexToI (x1:-s@(x2:-_)) (i:-is) = i*x2 + indexToI s is
indexToI _ _ = 0

iToIndex :: FormattedShape n -> Int -> Index n
iToIndex shape i = fst (f shape)
  where
    f :: FormattedShape m -> (Index m, Int)
    f N = (N, 0)
    f (s:-N) =
        let val = i `rem` s
        in
        (val:-N, val)
    f (s2:-s1:-ss) =
        let (index, a) = f (s1:-ss)
            newA :: Int
            newA = (i-a) `rem` s2
        in
        (div newA s1 :- index, newA)

unsafeGenerate :: G.Vector v a => Shape n -> (Index n -> a) -> Tensor v a n
unsafeGenerate shape f =
    Tensor (G.generate vecSize (g . iToIndex formatted)) formatted
  where
    vecSize = case formatted of
        N -> 1
        s:-_ -> s
    formatted = formatShape shape
    g index =
        INTERNAL_CHECK(inBoundsFormatted shape index,
                      "unsafeGenerate", (shape, index))
            $ f index

generate :: G.Vector v a => Shape n -> (Index n -> a) -> Tensor v a n
generate = unsafeGenerate

shape :: G.Vector v a => Tensor v a n -> Shape n
shape (Tensor _ s) = unformatShape s

convert :: (G.Vector v a, G.Vector w a) => Tensor v a n -> Tensor w a n
convert (Tensor vec shape) = Tensor (G.convert vec) shape

unsafeIndex :: (G.Vector v a) => Tensor v a n -> Index n -> a
unsafeIndex (Tensor vec shape) index =
    UNSAFE_CHECK(inBoundsFormatted shape index, "unsafeIndex", (shape, index))
        $ G.unsafeIndex vec (indexToI shape index)

index :: (G.Vector v a) => Tensor v a n -> Index n -> a
index t@(Tensor _ shape) index =
    BOUNDS_CHECK(inBoundsFormatted shape index, "index", (shape, index))
        $ unsafeIndex t index

maybeIndex :: (G.Vector v a) => Tensor v a n -> Index n -> Maybe a
maybeIndex t@(Tensor _ shape) index
  | inBoundsFormatted shape index = Just $ unsafeIndex t index
  | otherwise = Nothing

scalar :: G.Vector v a => Tensor v a 0 -> a
scalar (Tensor vec shape) =
    INTERNAL_CHECK(G.length vec == 1, "scalar", (G.length vec, shape))
        $ G.unsafeIndex vec 0

tensor :: G.Vector v a => a -> Tensor v a 0
tensor x = Tensor (G.cons x G.empty) N

unsafeAdd :: (Num a, G.Vector v a) => Tensor v a n -> Tensor v a n -> Tensor v a n
unsafeAdd (Tensor v1 shape1) (Tensor v2 shape2) =
    UNSAFE_CHECK(shape1 == shape2 && G.length v1 == G.length v2,
                "unsafeAdd",
                (shape1, shape2))
        $ Tensor (G.zipWith (+) v1 v2) shape1

add :: (Num a, G.Vector v a) => Tensor v a n -> Tensor v a n -> Tensor v a n
add t1@(Tensor v1 shape1) t2@(Tensor v2 shape2) =
    BOUNDS_CHECK(shape1 == shape2 && G.length v1 == G.length v2,
                "add",
                (shape1, shape2))
        $ unsafeAdd t1 t2

tensorProduct
    :: forall v a (m::Nat) (n::Nat). (Num a, G.Vector v a)
    => Tensor v a n
    -> Tensor v a m
    -> Tensor v a (n+m)
tensorProduct (Tensor v1 shape1) (Tensor v2 shape2) = Tensor newVec newShape
  where
    newVec = G.concat $ map (\x -> G.map (*x) v2) (G.toList v1)
    newShape :: Shape (n+m)
    newShape = case shape2 of
        N -> shape1
        (hd:-_) -> fmap (*hd) shape1 SizedList.++ shape2
{-# INLINE[1] tensorProduct #-}

{-# RULES "tensorProduct/scalarScalar" tensorProduct = tensorProductScalarScalar #-}
{-# RULES "tensorProduct/scalar" tensorProduct = tensorProductScalar #-}

tensorProductScalar
    :: forall v a (m::Nat). (Num a, G.Vector v a)
    => Tensor v a 0
    -> Tensor v a m
    -> Tensor v a m
tensorProductScalar (Tensor v1 _) (Tensor v2 shape) = Tensor newVec shape
  where
    newVec = G.map (* (G.unsafeIndex v1 0)) v2
{-# INLINE tensorProductScalar #-}

tensorProductScalarScalar
    :: forall v a. (Num a, G.Vector v a)
    => Tensor v a 0
    -> Tensor v a 0
    -> Tensor v a 0
tensorProductScalarScalar (Tensor v1 _) (Tensor v2 _) = Tensor newVec N
  where
    newVec = G.cons (G.unsafeIndex v1 0 * G.unsafeIndex v2 0) G.empty
{-# INLINE tensorProductScalarScalar #-}

unsafeContract
    :: forall v a (n::Nat). (Num a, G.Vector v a)
    => Tensor v a (n+2)
    -> Int
    -> Int
    -> Tensor v a n
unsafeContract (Tensor vec fShape) i j =
    -- uNSAFE_CHECK(0 <= i && i < j && j < SizedList.length fShape && iSize == div j' if j >= G.length vecFShape then 1 else G.unsafeIndex vecFShape j, "unsafeContract", (fShape, i, j))
    id
        $ Tensor newVector newFShape
  where
    vecFShape :: U.Vector Int
    vecFShape = sizedListToVector fShape
    i' = G.unsafeIndex vecFShape i
    iSize = div i' $ G.unsafeIndex vecFShape (i+1)
    j' = G.unsafeIndex vecFShape i
    ipj = i'+j'
    others = G.sum vecFShape - ipj
    resultSize = fromMaybe 1 $ shapeBeforeI G.!? i
    shapeBeforeI = G.map (flip div i') $ G.slice 0 i vecFShape 
    shapeBetween = G.map (flip div j') $ G.slice (i+1) (j-i-1) vecFShape
    shapeAfterJ = G.slice (j+1) (G.length vecFShape - j - 1) vecFShape
    newFShape :: FormattedShape n
    newFShape =
        unsafeVectorToSizedList
            (shapeBeforeI G.++ shapeBetween G.++ shapeAfterJ)
    sumStartingAt i =
        let vec' :: v a
            vec' = G.generate iSize (\k -> vec G.! (i+ipj*k))
        in
        trace (show (iSize, i, ipj)) $
        G.sum vec'
    newVector = G.generate resultSize (\i -> sumStartingAt (i*others))

-- unsafeContract
--     :: (Num a, G.Vector v a, KnownNat m, KnownNat n, 0 < m, m < n, n < p+2)
--     => Tensor v a (p+2)
--     -> Proxy m
--     -> Proxy n
--     -> Tensor v a p
-- unsafeContract proxyM proxyN (Tensor vec fShape) i j =
--     UNSAFE_CHECK(0 <= i && i < j && j < SizedList.length fShape,
--                 "unsafeContract", (fShape, i, j))
--         $ undefined
--   where
--     (

contract
    :: (Num a, G.Vector v a)
    => Tensor v a (n+2)
    -> YesNoList (n+2) 2
    -> Tensor v a n
contract (Tensor vec fShape) yn = Tensor undefined newFShape
  where
    newFShape = dropYes fShape yn

dropYes :: FormattedShape (m+n) -> YesNoList (m+n) n -> FormattedShape m
dropYes fShape yn = fst $ f fShape yn
  where
    f :: forall (p::Nat) (q::Nat).
        FormattedShape (p+q) -> YesNoList (p+q) q -> (FormattedShape p, Int)
    f N E = (N, 1)
    f (x:-fShape) (No yns) =
        let (newFShape, i) = f (unsafeCoerce fShape) (unsafeCoerce yns)
        in
        unsafeCoerce (div x i :- newFShape, i)
    f (x:-fShape) (Yes yns) =
        let (newFShape, i) = f (unsafeCoerce fShape) (unsafeCoerce yns)
        in
        unsafeCoerce (newFShape, i*x)
    f _ _ = error "dropYes: can't happen"

changeBasis
    :: (Num a, G.Vector v a)
    => Tensor v a n
    -> YesNoList n m
    -> Tensor v a 2
    -> Tensor v a n
changeBasis = undefined

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
    listShape _ _ = BOUNDS_CHECK(False, "shapeF", "") $ undefined

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
        let shape = listShape (Proxy::Proxy a) lst
        in
        Tensor (G.fromList $ flatten (Proxy::Proxy n) lst) (formatShape shape)

    toList (Tensor vec fShape) =
        shapeList (unformatShape fShape) (G.toList vec)
