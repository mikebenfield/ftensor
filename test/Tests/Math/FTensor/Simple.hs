{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.Math.FTensor.Simple (
    tests
) where

import Prelude hiding (head)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck hiding (generate, scale)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.TH (testGroupGenerator)
import qualified Test.Tasty.SmallCheck as SC

import Test.SmallCheck.Series hiding (generate)

import qualified Data.Vector.Generic as G
import qualified Data.Vector as V
import Data.Functor.Identity (Identity)

import Math.FTensor.Core
import Math.FTensor.Simple

import Data.Proxy

newtype Vec1 a = Vec1 (SimpleTensor a 1)
     deriving Show
newtype Vec2 a = Vec2 (SimpleTensor a 1)
    deriving Show
newtype Vec3 a = Vec3 (SimpleTensor a 1)
    deriving Show
newtype Vec4 a = Vec4 (SimpleTensor a 1)
    deriving Show

-- newtype Mat11 a = Mat11 (FTV.MatV a)
--     deriving Show
-- newtype Mat22 a = Mat22 (FTV.MatV a)
--     deriving Show

instance (Serial m a) => Serial m (Vec1 a) where
    series = cons1 f
      where
        f i = Vec1 $ generate [1] (\_ -> i)

instance (Serial m a) => Serial m (Vec2 a) where
    series = cons2 f
      where
        f :: a -> a -> Vec2 a
        f i j = Vec2 $ generate [2] (([i, j] !!) . G.head)

instance (Serial m a) => Serial m (Vec3 a) where
    series = cons3 f
      where
        f :: a -> a -> a -> Vec3 a
        f i j k = Vec3 $ generate [3] (([i,j,k] !!) . G.head)

instance (Serial m a) => Serial m (Vec4 a) where
    series = cons4 f
      where
        f :: a -> a -> a -> a -> Vec4 a
        f i j k l = Vec4 $ generate [4] (([i,j,k,l] !!) . G.head)

-- instance (Serial m a) => Serial m (Mat11 a) where
--     series = cons1 f
--       where
--         f :: a -> Mat11 a
--         f i = Mat11 $ generate [1, 1] (\_ -> i)

-- instance (Serial m a) => Serial m (Mat22 a) where
--     series = cons4 f
--       where
--         f :: a -> a -> a -> a -> Mat22 a
--         f i j k l =
--             let g indices =
--                     case (indices G.! 0, indices G.! 1) of
--                         (0, 0) -> i
--                         (0, 1) -> j
--                         (1, 0) -> k
--                         _ -> l
--             in
--             Mat22 $ generate [2, 2] g

autoTests = $(testGroupGenerator)

tests = testGroup "tests" [autoTests]

v1 :: SimpleTensor Int 1
v1 = generate [2] G.head

case_index_1 = (index v1 [0], index v1 [1]) @?= (0, 1)

case_shape_1 = shape v1 @?= [2]

v2 :: SimpleTensor Int 0
v2 = generate [] (\_ -> 0)

case_index_2 = index v2 [] @?= 0

case_shape_2 = shape v2 @?= []

v3 :: SimpleTensor Int 2
v3 = generate [2, 3] (\v -> (v G.! 0)^(v G.! 1))

case_index_3 =
    (index v3 [0, 0], index v3 [0, 1], index v3 [0, 2],
     index v3 [1, 0], index v3 [1, 1], index v3 [1, 2])
    @?= (1, 0, 0, 1, 1, 1)

case_shape_3 = shape v3 @?= [2, 3]

-- v1 :: FTV.VecV Int
-- v1 = generate (5:-Nil) (\(i:-Nil) -> i)

-- v2 :: FTV.VecV Int
-- v2 = generate (5:-Nil) (\(i:-Nil) -> 20*i)

-- case_unformatDimVector_1 =
--     FTV.unformatDimVector 24 [6, 3] @?= [4, 2, 3]

-- case_unformatDimVector_2 =
--     FTV.unformatDimVector 24 [] @?= [24]

-- case_unformatDimVector_3 =
--     FTV.unformatDimVector 0 [] @?= []

-- case_formatDimVector_1 =
--     FTV.formatDimVector [4, 2, 3] @?= (24, [6, 3])

-- case_formatDimVector_2 =
--     FTV.formatDimVector [24] @?= (24, [])

-- case_formatDimVector_3 =
--     FTV.formatDimVector [] @?= (1, [])

-- case_unformatIndex_1 =
--     FTV.unformatIndex [6, 3] [1, 0, 0] @?= 6

-- case_unformatIndex_2 =
--     FTV.unformatIndex [6] [1, 0] @?= 6

-- case_unformatIndex_3 =
--     FTV.unformatIndex [] [7] @?= 7

-- case_unformatIndex_4 =
--     FTV.unformatIndex [] [] @?= 0

-- case_formatIndex_1 =
--     FTV.formatIndex 12 [6, 3] 6 @?= [1, 0, 0]

-- case_formatIndex_2 =
--     FTV.formatIndex 7 [] 1 @?= [1]

-- case_formatIndex_3 =
--     FTV.formatIndex 16 [8] 9 @?= [1, 1]

-- case_formatIndex_4 =
--     FTV.formatIndex 0 [] 0 @?= []

-- case_blarg =
--     let m :: FTV.MatV Int
--         m = FTV.TensorV Proxy [2] [0,0,0,1]
--         c :: FTV.ScalarV Int
--         c = contract m 0 1
--     in
--     zeroDim (trace m) @?= c
