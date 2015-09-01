{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Math.FTensor.Internal (
    Shape,
    MultiIndex,
    Offsets,
    ShapeVector,
    MultiIndexVector,

    shapeVectorToOffsets,
    shapeToOffsets,
    offsetsToShapeVector,
    unsafeOffsetsToShape,
    multiIndexVectorToIndex,
    multiIndexToIndex,
    validShape,
    inBoundsVector,
    inBoundsOffsets,
    inBounds,
    indexToMultiIndexVector,
    unsafeIndexToMultiIndex,
) where

import GHC.TypeLits

import qualified Data.Vector.Unboxed as U

import Math.FTensor.SizedList

type Shape (n::Nat) = SizedList n Int

type MultiIndex (n::Nat) = SizedList n Int

type Offsets = U.Vector Int

type ShapeVector = U.Vector Int

type MultiIndexVector = U.Vector Int

shapeVectorToOffsets :: ShapeVector -> (Int, Offsets)
shapeVectorToOffsets sv = (U.head result, U.force $ U.tail result)
  where
    result = U.scanr' (*) 1 sv

shapeToOffsets :: Shape n -> (Int, Offsets)
shapeToOffsets = shapeVectorToOffsets . sizedListToVector

offsetsToShapeVector :: Int -> Offsets -> ShapeVector
offsetsToShapeVector length offsets =
    U.zipWith div (U.cons length offsets) offsets

unsafeOffsetsToShape :: Int -> Offsets -> Shape n
unsafeOffsetsToShape length offsets =
    unsafeVectorToSizedList $ offsetsToShapeVector length offsets

multiIndexVectorToIndex :: Offsets -> MultiIndexVector -> Int
multiIndexVectorToIndex offsets multiIndexVector =
    U.sum $ U.zipWith (*) offsets multiIndexVector

multiIndexToIndex :: Offsets -> MultiIndex n -> Int
multiIndexToIndex offsets multiIndex =
    multiIndexVectorToIndex offsets (sizedListToVector multiIndex)

validShape :: Shape n -> Bool
validShape N = True
validShape (x:-xs) = x > 0 && validShape xs

inBoundsVector :: ShapeVector -> MultiIndexVector -> Bool
inBoundsVector shapeVector multiIndexVector =
    U.and $ U.zipWith (\slotSize i -> 0 <= i && i < slotSize)
        shapeVector multiIndexVector

inBoundsOffsets :: Int -> Offsets -> MultiIndex n -> Bool
inBoundsOffsets length offsets multiIndex =
    inBoundsVector
        (offsetsToShapeVector length offsets)
        (sizedListToVector multiIndex)

inBounds :: Shape n -> MultiIndex n -> Bool
inBounds shape multiIndex =
    inBoundsVector (sizedListToVector shape) (sizedListToVector multiIndex)

-- the algorithm is:
-- Suppose the index 
-- j = offset0 * i0 + offset1 * i1 + offset2 * i2
-- a2 = j `rem` o1 = o2 * i2
-- i2 = a2 `div` o2
-- a1 = (j - a2) `rem` o0 = o1 * i1
-- i1 = a1 `div` o1
-- a0 = (j - a1) = a0 * i0
-- i0 = a0 `div` o0
indexToMultiIndexVector :: Offsets -> Int -> MultiIndexVector
indexToMultiIndexVector offsets j = U.zipWith div as offsets
  where
    modifiedOffsets = U.cons maxBound $ U.init offsets
    as = U.postscanr' (\s a -> (j-a) `rem` s) 0 modifiedOffsets

unsafeIndexToMultiIndex :: Offsets -> Int -> MultiIndex n
unsafeIndexToMultiIndex offsets j =
    unsafeVectorToSizedList $ indexToMultiIndexVector offsets j
