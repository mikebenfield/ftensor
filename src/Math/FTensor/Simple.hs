
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Math.FTensor.Simple (
    SimpleTensor
) where

import GHC.TypeLits
import Unsafe.Coerce

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G

import Math.FTensor.Core

data SimpleTensor a (n::Nat) where
    Z :: a -> SimpleTensor a 0
    S :: V.Vector (SimpleTensor a n) -> SimpleTensor a (n+1)

deriving instance Show a => Show (SimpleTensor a n)

instance Eq a => Eq (SimpleTensor a n) where
    (==) = eq

eq :: Eq a => SimpleTensor a (n::Nat) -> SimpleTensor a (n::Nat) -> Bool
eq (Z v) (Z w) = v == w
eq (S vec1) (S vec2) = vec1 == (unsafeCoerce vec2)
eq _ _ = error "eq"

basicShape_ :: SimpleTensor a (n::Nat) -> Shape
basicShape_ (Z _) = U.empty
basicShape_ (S vec) = G.cons (G.length vec) (basicShape_ $ G.head vec)

basicIndex_ :: SimpleTensor a (n::Nat) -> IndexVector -> a
basicIndex_ (Z val) _ = val
basicIndex_ (S vec) indexVector =
    basicIndex_ (vec G.! (G.head indexVector)) (G.tail indexVector)

-- unfortunately it seems there is no way to implement this without
-- unsafeCoerce
basicGenerate_ :: Shape -> (IndexVector -> a) -> SimpleTensor a n
basicGenerate_ shape f
  | U.length shape == 0 = unsafeCoerce $ Z $ f U.empty
  | otherwise =
      unsafeCoerce . S $ V.generate (G.head shape)
        (\i -> basicGenerate_ (G.tail shape) (g i))
  where
    g i indexVector = f $ G.cons i indexVector

instance KnownNat n => Tensor (SimpleTensor a n) where
    type SlotCount (SimpleTensor a n) = n
    type Scalar (SimpleTensor a n) = a
    basicShape = basicShape_
    basicIndex = basicIndex_
    basicGenerate = basicGenerate_
