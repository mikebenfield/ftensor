{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Tests.Math.FTensor.Internal (
    tests
) where

import Data.Proxy
import GHC.Exts

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.TH (testGroupGenerator)

import qualified Data.Vector.Generic as G
import qualified Data.Vector as V

import Math.FTensor.Internal
import Math.FTensor.SizedList

tests = testGroup "Tests.Math.FTensor.Internal" [smallCheckProperties, autoTests]

autoTests = $(testGroupGenerator)

case_shapeToOffsets_1 =
    shapeToOffsets (5:-3:-2:-N) @?= (30, G.fromList [6,2,1])
case_shapeToOffsets_2 =
    shapeToOffsets N @?= (1, G.fromList [])
case_shapeToOffsets_3 =
    shapeToOffsets (17:-N) @?= (17, G.fromList [1])

case_unsafeOffsetsToShape_1 = unsafeOffsetsToShape 1 (G.fromList []) @?= N
case_unsafeOffsetsToShape_2 =
    unsafeOffsetsToShape 17 (G.fromList [1]) @?= (17:-N)
case_unsafeOffsetsToShape_3 =
    unsafeOffsetsToShape 30 (G.fromList [6,2,1]) @?= (5:-3:-2:-N)

case_multiIndexToIndex_1 =
    multiIndexToIndex (G.fromList [6,2,1]) (2:-1:-3:-N) @?= 17

case_inBounds_1 =
    inBounds (3:-3:-N) (1:-2:-N) @?= True
case_inBounds_2 =
    inBounds (3:-3:-N) (1:-3:-N) @?= False
case_inBounds_3 =
    inBounds (3:-3:-N) (1:- -1:-N) @?= False

case_indexToMultiIndexVector_1 =
    indexToMultiIndexVector (G.fromList [15, 3, 1]) 2 @?= 
        (G.fromList [0,0,2])
case_indexToMultiIndexVector_2 =
    indexToMultiIndexVector (G.fromList [15, 3, 1]) 3 @?= 
        (G.fromList [0,1,0])
case_indexToMultiIndexVector_3 =
    indexToMultiIndexVector (G.fromList [15, 3, 1]) 15 @?= 
        (G.fromList [1,0,0])

-- case_validShape_1 = validShape (Proxy::Proxy 0) [1] @?= False

-- case_validShape_1 = validShape (0:-N) @?= False
-- case_validShape_2 = validShape (1:-N) @?= True

-- case_formatShape_1 = formatShape (5:-N) @?= (5:-N)
-- case_formatShape_2 = formatShape (3:-5:-N) @?= (15:-5:-N)

-- case_indexToI_1 = indexToI (5:-N) (3:-N) @?= 3
-- case_indexToI_2 = indexToI (15:-5:-N) (2:-3:-N) @?= 13

-- case_iToIndex_1 = iToIndex (5:-N) 3 @?= (3:-N)
-- case_iToIndex_2 = iToIndex (15:-5:-N) 13 @?= (2:-3:-N)

-- m1 :: TensorV Int 2
-- m1 = fromList [[1, 2], [3, 4]]

-- case_unsafeContract_1 = unsafeContract m1 0 1 @?= tensor 5

-- newtype T0 = T0 (TensorV Int 0)
--     deriving (Show, Eq)

-- instance (Monad m) => Serial m T0 where
--     series = cons1 (T0 . tensor)

-- newtype T3 = T3 (TensorV Int 1)
--     deriving (Show, Eq)

-- instance (Monad m) => Serial m T3 where
--     series = cons3 f
--       where
--         f :: Int -> Int -> Int -> T3
--         f i j k = T3 $ fromList [i, j, k]

-- newtype T2 = T2 (TensorV Int 1)
--     deriving (Show, Eq)

-- instance (Monad m) => Serial m T2 where
--     series = cons2 f
--       where
--         f :: Int -> Int -> T2
--         f i j = T2 $ fromList [i, j]

-- newtype T22 = T22 (TensorV Int 2)
--     deriving (Show, Eq)

-- instance (Monad m) => Serial m T22 where
--     series = cons4 f
--       where
--         f :: Int -> Int -> Int -> Int -> T22
--         f i j k l = T22 $ fromList [[i, j], [k,l]]

-- newtype T311 = T311 (TensorV Int 3)
--     deriving (Show, Eq)

-- instance (Monad m) => Serial m T311 where
--     series = cons3 f
--       where
--         f :: Int -> Int -> Int -> T311
--         f i j k = T311 $ fromList [[[i]], [[j]], [[k]]]

smallCheckProperties = testGroup "SmallCheck" []
--     [ SC.testProperty "tensor . scalar" $
--         \(T0 t) -> t == (tensor . scalar) t
--     , SC.testProperty "fromList . toList 1" $
--         \(T3 t) -> t == (fromList . toList) t
--     , SC.testProperty "fromList . toList 2" $
--         \(T311 t) -> t == (fromList . toList) t
--     , SC.testProperty "tensorProduct scaling 1" $
--         \(T0 t1, T0 t2) ->
--             tensorProduct t1 t2 == tensor (scalar t1 * scalar t2)
--     , SC.testProperty "tensorProduct scaling 2" $
--         \(T0 t1, T311 t2) ->
--             let s = scalar t1
--                 idx i = index t2 (i:-0:-0:-N)
--             in
--             tensorProduct t1 t2 ==
--                 fromList
--                     [ [[s*idx 0]]
--                     , [[s*idx 1]]
--                     , [[s*idx 2]]
--                     ]
--     , SC.testProperty "tensorProduct 1" $
--         \(T2 t1, T2 t2) ->
--             let f i j = index t1 (i:-N) * index t2 (j:-N)
--             in
--             tensorProduct t1 t2 == fromList
--                 [ [f 0 0, f 0 1]
--                 , [f 1 0, f 1 1]
--                 ]
--     ]
