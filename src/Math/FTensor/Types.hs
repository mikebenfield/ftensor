{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Math.FTensor.Types (
    -- ScalarV,
    -- TensorV(..),
    -- VecV,
    -- MatV,
    -- -- VecU,
    -- -- VectorMat,
    -- -- MatU,
    -- -- VectorTensor,
    -- unformatDimVector, -- XXX
    -- formatDimVector,
    -- unformatIndex,
    -- formatIndex,
) where

-- import Data.Proxy
-- import GHC.TypeLits

-- import qualified Data.Vector as V
-- import qualified Data.Vector.Generic as G
-- import qualified Data.Vector.Unboxed as U

-- import Math.FTensor.Core

-- unformatDimVector0 :: forall (n::Nat). Proxy n -> Int -> DimVector -> DimVector
-- unformatDimVector0 = undefined

-- unformatDimVector :: Int -> DimVector -> DimVector
-- unformatDimVector 0 _ = U.empty
-- unformatDimVector prod formattedDimVector =
--     let v1 = U.snoc (U.cons prod formattedDimVector) 1
--     in
--     U.generate (1 + U.length formattedDimVector)
--         (\i -> U.unsafeIndex v1 i `div` U.unsafeIndex v1 (i+1))

-- formatDimVector :: DimVector -> (Int, DimVector)
-- formatDimVector dimVector
--   | len == 0 = (1, U.empty)
--   | otherwise = (U.head first, rem)
--   where
--     (first, rem) = U.splitAt 1 $ U.reverse $ U.unfoldrN len f (1, len - 1)
--     len = U.length dimVector
--     f (lastVal, i) =
--         let thisVal = lastVal * U.unsafeIndex dimVector i
--         in
--         Just (thisVal, (thisVal, i - 1))

-- unformatIndex :: DimVector -> IndexVector -> Int
-- unformatIndex dimVector indexVector =
--     U.sum $ U.zipWith (*) (U.snoc dimVector 1) indexVector

-- -- Suppose dimVector is [np, p]
-- -- Then i = np * x1 + p * x2 + x3
-- -- x3 = a0 `rem` b0 `div` c0
-- -- x2 = a1 `rem` b1 `div` c1
-- -- etc, where a0 = i, and a(j+1) = i - aj `rem` bj
-- --            bj = dimVector (lastIndex - j)
-- --            c0 = 1, cj = dimVector (lastIndex - j + 1)
-- formatIndex :: DimVector -> Int -> IndexVector
-- formatIndex 0 _ = U.empty
-- formatIndex dimVector i =
--     U.reverse $ U.unfoldrN (len + 1) f (i, 1, len)
--   where
--     len = U.length dimVector
--     dimVector' = U.cons maxBound dimVector
--     f (a, c, k) =
--         let b = (U.!) dimVector' k
--             step = a `rem` b
--             nextA = i - step
--         in
--         Just (step `div` c, (nextA, b, k - 1))

-- -- The second parameter to the constructor stores information about the
-- -- dimensions in the following format. Let L be the length of the final
-- -- parameter.
-- -- DimCount = 0: empty
-- -- DimCount = 1: empty (get it from the length of the last parameter)
-- -- DimCount = 2, mxn : [n] (then m = div L n)
-- -- DimCount = 3, mxnxp : [np, p] (then n = np / p, and m = L / (np).
-- -- this is to optimize the presumably more common operation unsafeIndex at the
-- -- expense of the less common dimensions.
-- data TensorV a (n::Nat) =
--     TensorV
--         {-# UNPACK #-} !(Proxy n)
--         {-# UNPACK #-} !(U.Vector Int)
--         {-# UNPACK #-} !(V.Vector a)
--     deriving (Show, Eq)
 
-- instance (KnownNat n) => Tensor (TensorV a n) where
--     type DimCount (TensorV a n) = n
--     type Scalar (TensorV a n) = a
--     dimensions = \(TensorV _ dims vec) -> unformatDimVector (G.length vec) dims
--     {-# INLINE dimensions #-}
--     basicUnsafeIndex = \(TensorV _ dims vec) indices ->
--         (G.!) vec $ unformatIndex dims indices
--     {-# INLINE basicUnsafeIndex #-}
--     generate = \dimVector f ->
--         let (len, formattedDimVector) = formatDimVector dimVector
--         in
--         TensorV
--             Proxy
--             formattedDimVector
--             (G.generate len (f . formatIndex len formattedDimVector))
--     {-# INLINE generate #-}

-- -- data TensorU (n::Nat) a =
-- --     TensorU
-- --         {-# UNPACK #-} !(Proxy n)
-- --         {-# UNPACK #-} !(U.Vector Int)
-- --         {-# UNPACK #-} !(U.Vector a)

-- -- data family TensorVOpen (n::Nat) a

-- -- newtype instance TensorVOpen 0 a = TensorVOpen0 a

-- -- data instance TensorVOpen 1 a =
-- --     TensorVOpen1
-- --         {-# UNPACK #-} !Int
-- --         {-# UNPACK #-} !(V.Vector a)

-- -- data instance TensorVOpen 2 a =
-- --     TensorVOpen2
-- --         {-# UNPACK #-} !Int
-- --         {-# UNPACK #-} !Int
-- --         {-# UNPACK #-} !(V.Vector a)

-- -- data instance TensorVOpen 3 a =
-- --     TensorVOpen3
-- --         {-# UNPACK #-} !Int
-- --         {-# UNPACK #-} !Int
-- --         {-# UNPACK #-} !Int
-- --         {-# UNPACK #-} !(V.Vector a)

-- -- data instance TensorVOpen 4 a =
-- --     TensorVOpen4
-- --         {-# UNPACK #-} !Int
-- --         {-# UNPACK #-} !Int
-- --         {-# UNPACK #-} !Int
-- --         {-# UNPACK #-} !Int
-- --         {-# UNPACK #-} !(V.Vector a)

-- -- newtype ZeroDim a = ZeroDim a
-- --     deriving (Eq, Show)

-- -- instance Tensor (ZeroDim a) where
-- --     type DimCount (ZeroDim a) = 0
-- --     type Scalar (ZeroDim a) = a
-- --     dimensions = (\_ -> Nil)
-- --     {-# INLINE dimensions #-}
-- --     basicUnsafeIndex = (\(ZeroDim x) _ -> x)
-- --     {-# INLINE basicUnsafeIndex #-}
-- --     generate = \_ f -> ZeroDim $ f Nil
-- --     {-# INLINE generate #-}

-- -- data VectorVec v a = VectorVec !(v a)
-- --     deriving (Eq, Show)

-- -- instance G.Vector v a => Tensor (VectorVec v a) where
-- --     type DimCount (VectorVec v a) = 1
-- --     type Scalar (VectorVec v a) = a
-- --     dimensions = \(VectorVec vector) -> G.length vector :- Nil
-- --     {-# INLINE dimensions #-}
-- --     basicUnsafeIndex = \(VectorVec vector) (i:-Nil) -> G.unsafeIndex vector i
-- --     {-# INLINE basicUnsafeIndex #-}
-- --     generate = \(d:-Nil) f -> VectorVec $ G.generate d (\i -> f (i:-Nil))
-- --     {-# INLINE generate #-}

-- type ScalarV a = TensorV a 0

-- type VecV a = TensorV a 1

-- type MatV a = TensorV a 2

-- -- type VecU a = VectorVec U.Vector a

-- -- data VectorMat v a = VectorMat {-# UNPACK #-} !Int !(v a)
-- --     deriving (Eq, Show)

-- -- instance G.Vector v a => Tensor (VectorMat v a) where
-- --     type DimCount (VectorMat v a) = 2
-- --     type Scalar (VectorMat v a) = a
-- --     dimensions = \(VectorMat nCols vector) ->
-- --         (G.length vector `div` nCols) :- nCols :- Nil
-- --     {-# INLINE dimensions #-}
-- --     basicUnsafeIndex = \(VectorMat nCols vector) (i:-j:-Nil) ->
-- --         G.unsafeIndex vector (nCols*i + j)
-- --     {-# INLINE basicUnsafeIndex #-}
-- --     generate = \(nRows:-nCols:-Nil) f ->
-- --         VectorMat nCols $ G.generate (nRows*nCols)
-- --             (\k -> f (div k nCols :- rem k nCols :- Nil))
-- --     {-# INLINE generate #-}


-- -- type MatU a = VectorMat U.Vector a

-- -- data VectorTensor v (n::Nat) a = VectorTensor !(DimList n) !(v a)

-- -- instance (KnownNat n, G.Vector v a) => Tensor (VectorTensor v n a) where
-- --     type DimCount (VectorTensor v n a) = n
-- --     type Scalar (VectorTensor v n a) = a
-- --     dimensions = undefined
-- --     -- \(VectorTensor dims vector) ->
-- --     --     let prod = G.product dims
-- --     --     in
-- --     --     unsafeCoerce $
-- --     --         prod `div` G.length vector :- unsafeVectorToSizedList dims
-- --     {-# INLINE dimensions #-}
-- --     basicUnsafeIndex = undefined
-- --     {-# INLINE basicUnsafeIndex #-}
-- --     generate = undefined
-- --     {-# INLINE generate #-}
