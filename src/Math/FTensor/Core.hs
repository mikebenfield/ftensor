{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Math.FTensor.Core (
    Tensor(..),

    Shape,
    IndexVector,
    validShape,
    inBounds,
    shape,
    unsafeIndex,
    index,
    maybeIndex,
    unsafeGenerate,
    generate,

    SameScalar,
    SameScalar3,
    SameSlotCount,
    SameSlotCount3,

    convert,

    scale',
    add',
    tensorMultiply',
    unsafeContract',
    contract',
    changeBasis',
    changeBasisAll',

    TensorN,
    scale,
    add,
    tensorMultiply,
    unsafeContract,
    contract,
    changeBasis,
    changeBasisAll,
) where

import Data.Maybe (fromMaybe)
import Data.Proxy
import GHC.TypeLits

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V

#include "ftensor.h"

class KnownNat (SlotCount t) => Tensor t where
    type SlotCount t :: Nat
    type Scalar t
    basicShape :: t -> Shape
    basicIndex :: t -> IndexVector -> Scalar t
    basicGenerate :: Shape -> (IndexVector -> Scalar t) -> t

type Shape = U.Vector Int

type IndexVector = U.Vector Int

validShape :: KnownNat n => Proxy n -> Shape -> Bool
validShape p shape =
    U.length shape == fromIntegral (natVal p) &&
        U.and (U.map (0<) shape)

inBounds :: Shape -> IndexVector -> Bool
inBounds shape indexVector =
    U.length shape == U.length indexVector &&
        U.and (U.zipWith (<) indexVector shape) &&
            U.and (U.map (0<=) indexVector)

shape :: forall t. Tensor t => t -> Shape
shape x = INTERNAL_CHECK(len == U.length result, "shape", (len, result)) $
    result
  where
    len :: Int
    len = fromIntegral $ natVal (Proxy::Proxy (SlotCount t))
    result = basicShape x

unsafeIndex :: Tensor t => t -> IndexVector -> Scalar t
unsafeIndex x indices =
    UNSAFE_CHECK(inBounds (shape x) indices, "unsafeIndex", (indices, shape x)) $
        basicIndex x indices

index :: Tensor t => t -> IndexVector -> Scalar t
index x indices =
    BOUNDS_CHECK(inBounds (shape x) indices, "index", (indices, shape x)) $
        unsafeIndex x indices

maybeIndex :: Tensor t => t -> IndexVector -> Maybe (Scalar t)
maybeIndex x indices
  | inBounds (shape x) indices = Just (unsafeIndex x indices)
  | otherwise = Nothing

unsafeGenerate :: forall t. Tensor t => Shape -> (IndexVector -> Scalar t) -> t
unsafeGenerate shape f =
    basicGenerate shape g
  where
    g indices =
        INTERNAL_CHECK(inBounds shape indices, "generate", (indices, shape)) $
            f indices

generate :: forall t. Tensor t => Shape -> (IndexVector -> Scalar t) -> t
generate shape f =
    BOUNDS_CHECK(validShape p shape, "generate", (shape, natVal p)) $
        unsafeGenerate shape f
  where
    p = Proxy::Proxy (SlotCount t)

type SameScalar s t = (Tensor s, Tensor t, Scalar s ~ Scalar t)

type SameScalar3 s t u = (SameScalar s t, SameScalar s u)

type SameSlotCount s t = (SameScalar s t, SlotCount s ~ SlotCount t)

type SameSlotCount3 s t u = (SameSlotCount s t, SameSlotCount s u)

convert :: (SameSlotCount s t) => s -> t
convert x = generate (shape x) (unsafeIndex x)

unsafeAdd' :: (Num (Scalar s), SameSlotCount3 s t u) => s -> t -> u
unsafeAdd' x y =
    unsafeGenerate (shape x) (\i -> unsafeIndex x i + unsafeIndex y i)

add' :: (Num (Scalar s), SameSlotCount3 s t u) => s -> t -> u
add' x y =
    BOUNDS_CHECK(shapeX == shapeY, "add'", (shapeX, shapeY)) $
        unsafeAdd' x y
  where
    shapeX = shape x
    shapeY = shape y

scale' :: (SameSlotCount s t, Num (Scalar s)) => s -> Scalar s -> t
scale' x scalar = generate (shape x) ((*scalar) . unsafeIndex x)

tensorMultiply'
    :: forall s t u.
        (SameScalar3 s t u,
        Num (Scalar s),
        (SlotCount s + SlotCount t) ~ SlotCount u,
        (SlotCount t + SlotCount s) ~ SlotCount u)
    => s
    -> t
    -> u
tensorMultiply' x y = generate (shapeX U.++ shapeY)
    (\indices ->
        let (l, r) = U.splitAt slotCountX indices
        in
        unsafeIndex x l * unsafeIndex y r)
  where
    shapeX = shape x
    shapeY = shape y
    slotCountX = U.length shapeX

unsafeContract'
    :: (SameScalar s t,
       Num (Scalar s),
       (SlotCount t + 2) ~ SlotCount s)
    => s
    -> Int
    -> Int
    -> t
unsafeContract' x i j =
    UNSAFE_CHECK(i /= j && sameSize, "unsafeContract'", (i, j, shapeX)) $
    generate remainingDimVector
    -- Use Data.Vector because it fuses and is faster than lists
    (\indices -> V.sum $ V.map
        (\k -> unsafeIndex x (insert indices k)) (V.enumFromN 0 sizeI))
  where
    shapeX = shape x
    sameSize = fromMaybe False $ do
        sizeI <- shapeX U.!? i
        sizeJ <- shapeX U.!? j
        return $ sizeI == sizeJ
    (i', j') =
        case compare i j of
            LT -> (i, j)
            GT -> (j, i)
            EQ -> error ("contract: both indices " ++ show i)
    sizeI = shapeX U.! i
    remainingDimVector = U.ifilter (\k _ -> k /= i && k /= j) shapeX
    insert vec val =
        let (remaining, thirdChunk) = U.splitAt j' vec
            (firstChunk, secondChunk) = U.splitAt i' remaining
        in
        U.snoc firstChunk val U.++ U.snoc secondChunk val U.++ thirdChunk

contract'
    :: (SameScalar s t,
       Num (Scalar s),
       (SlotCount t + 2) ~ SlotCount s)
    => s
    -> Int
    -> Int
    -> t
contract' x i j =
    BOUNDS_CHECK(i /= j && sameSize, "contract'", (i, j, shapeX)) $
    unsafeContract' x i j
  where
    shapeX = shape x
    sameSize = fromMaybe False $ do
        sizeI <- shapeX U.!? i
        sizeJ <- shapeX U.!? j
        return $ sizeI == sizeJ

changeBasis'
    :: (SameSlotCount s t, SameScalar s m, Num (Scalar s), SlotCount m ~ 2)
    => s
    -> m
    -> U.Vector Int
    -> U.Vector Int
    -> t
changeBasis' x changeMatrix slots dualSlots = undefined

changeBasisAll'
    :: (SameSlotCount s t, SameScalar s m, Num (Scalar s), SlotCount m ~ 2)
    => s
    -> m
    -> t
changeBasisAll' = undefined

type TensorN t (n::Nat) = (Tensor (t n), SlotCount (t n) ~ n)

add :: (Tensor t, Num (Scalar t)) => t -> t -> t
add = add'

scale :: (Tensor t, Num (Scalar t)) => t -> Scalar t -> t
scale = scale'

tensorMultiply
    :: forall t (m::Nat) (n::Nat).
        (TensorN t m, TensorN t n,
        TensorN t (m+n),
        (n + m) ~ SlotCount (t (m+n)),
        SameScalar3 (t m) (t n) (t (m+n)),
        Num (Scalar (t m)))
    => t m
    -> t n
    -> t (m+n)
tensorMultiply = tensorMultiply'

unsafeContract
    :: forall t (m::Nat).
        (TensorN t m, TensorN t (m+2),
        SameScalar (t m) (t (m+2)),
        Num (Scalar (t m)))
    => t (m+2)
    -> Int
    -> Int
    -> t m
unsafeContract = unsafeContract

contract
    :: forall t (m::Nat).
        (TensorN t m, TensorN t (m+2),
        SameScalar (t m) (t (m+2)),
        Num (Scalar (t m)))
    => t (m+2)
    -> Int
    -> Int
    -> t m
contract = contract'

changeBasis
    :: (SameScalar t m, Num (Scalar t), SlotCount m ~ 2)
    => t
    -> m
    -> U.Vector Int
    -> U.Vector Int
    -> t
changeBasis = changeBasis'

changeBasisAll
    :: (SameScalar t m, Num (Scalar t), SlotCount m ~ 2)
    => t
    -> m
    -> t
changeBasisAll = changeBasisAll'

check :: Show b => Bool -> String -> String -> Int -> String -> b -> a -> a
check False checkType functionName lineNumber fileName otherData _ =
    error $ fileName ++ " (" ++ (show lineNumber) ++ "): Failed " ++
        checkType ++ " check in function " ++ functionName ++
            " with data " ++ (show otherData)
check _ _ _ _ _ _ val = val
