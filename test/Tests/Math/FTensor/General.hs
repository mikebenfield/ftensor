{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Tests.Math.FTensor.General (
    tests
) where

import Data.Proxy
import GHC.Exts (IsList(..))

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)
import Test.Tasty.TH (testGroupGenerator)
import qualified Test.Tasty.SmallCheck as SC

import Test.SmallCheck.Series hiding (generate, Positive)

import Math.FTensor.Algebra
import Math.FTensor.Lib.General
import Math.FTensor.General
import Math.FTensor.SizedList

import Math.FTensor.Lib.TypeList

autoTests = $(testGroupGenerator)

tests = testGroup "tests" [autoTests, smallCheckProperties]

v1 :: TensorBoxed '[3] Int
v1 = [1,2,3]

case_v1 :: Assertion
case_v1 = case v1 of
    Tensor arr -> arr @?= fromList [1::Int,2,3]

case_pIndex_1 = pIndex v1 (Proxy::Proxy '[0]) @?= 1
case_index_1 = index v1 (0:-N) @?= 1
case_pIndex_2 = pIndex v1 (Proxy::Proxy '[1]) @?= 2
case_index_2 = index v1 (1:-N) @?= 2
case_pIndex_3 = pIndex v1 (Proxy::Proxy '[2]) @?= 3
case_index_3 = index v1 (2:-N) @?= 3

m1 :: TensorBoxed '[3,3] Int
m1 = [[1,2,3], [4,5,6], [7,8,9]]

case_m1 = case m1 of
    Tensor arr -> arr @?= fromList [1::Int,2,3,4,5,6,7,8,9]

case_contract_1 =
    contract m1 (Proxy::Proxy 0) (Proxy::Proxy 1) @?= tensor 15

case_pIndex_4 = pIndex m1 (Proxy::Proxy '[0,0]) @?= 1
case_index_4 = index m1 (0:-0:-N) @?= 1
case_pIndex_5 = pIndex m1 (Proxy::Proxy '[0,1]) @?= 2
case_index_5 = index m1 (0:-1:-N) @?= 2
case_pIndex_6 = pIndex m1 (Proxy::Proxy '[0,2]) @?= 3
case_index_6 = index m1 (0:-2:-N) @?= 3
case_pIndex_7 = pIndex m1 (Proxy::Proxy '[1,0]) @?= 4
case_index_7 = index m1 (1:-0:-N) @?= 4
case_pIndex_8 = pIndex m1 (Proxy::Proxy '[1,1]) @?= 5
case_index_8 = index m1 (1:-1:-N) @?= 5
case_pIndex_9 = pIndex m1 (Proxy::Proxy '[1,2]) @?= 6
case_index_9 = index m1 (1:-2:-N) @?= 6
case_pIndex_10 = pIndex m1 (Proxy::Proxy '[2,0]) @?= 7
case_index_10 = index m1 (2:-0:-N) @?= 7
case_pIndex_11 = pIndex m1 (Proxy::Proxy '[2,1]) @?= 8
case_index_11 = index m1 (2:-1:-N) @?= 8
case_pIndex_12 = pIndex m1 (Proxy::Proxy '[2,2]) @?= 9
case_index_12 = index m1 (2:-2:-N) @?= 9

t1 :: TensorBoxed '[2,3,4] Int
t1 =
    [ [[1,2,3,4], [5,6,7,8], [9,10,11,12]]
    , [[13,14,15,16], [17,18,19,20], [21,22,23,24]]
    ]

case_t1 = case t1 of
    Tensor arr -> arr @?= fromList
        [1::Int,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24]

case_mul_1 =
    let t :: TensorBoxed '[4] Int
        t = [1,1,1,1]
    in
    mul t1 (Proxy::Proxy 2) t (Proxy::Proxy 0) @?= [[10, 26, 42], [58, 74, 90]]

case_mul_2 =
    let t :: TensorBoxed '[4] Int
        t = [1,1,1,1]
    in
    mul t (Proxy::Proxy 0) t1 (Proxy::Proxy 2) @?= [[10, 26, 42], [58, 74, 90]]

case_mul_3 =
    let t :: TensorBoxed '[3] Int
        t = [1,1,1]
    in
    mul t1 (Proxy::Proxy 1) t (Proxy::Proxy 0) @?=
        [[15, 18, 21, 24], [51, 54, 57, 60]]

case_index_13 = index t1 (0:-0:-0:-N) @?= 1
case_pIndex_13 = pIndex t1 (Proxy::Proxy '[0,0,0]) @?= 1
case_index_14 = index t1 (0:-0:-1:-N) @?= 2
case_pIndex_14 = pIndex t1 (Proxy::Proxy '[0,0,1]) @?= 2
case_index_15 = index t1 (0:-0:-2:-N) @?= 3
case_pIndex_15 = pIndex t1 (Proxy::Proxy '[0,0,2]) @?= 3
case_index_16 = index t1 (0:-0:-3:-N) @?= 4
case_pIndex_16 = pIndex t1 (Proxy::Proxy '[0,0,3]) @?= 4
case_index_17 = index t1 (0:-1:-0:-N) @?= 5
case_pIndex_17 = pIndex t1 (Proxy::Proxy '[0,1,0]) @?= 5
case_index_18 = index t1 (0:-1:-1:-N) @?= 6
case_pIndex_18 = pIndex t1 (Proxy::Proxy '[0,1,1]) @?= 6
case_index_19 = index t1 (0:-1:-2:-N) @?= 7
case_pIndex_19 = pIndex t1 (Proxy::Proxy '[0,1,2]) @?= 7
case_index_20 = index t1 (0:-1:-3:-N) @?= 8
case_pIndex_20 = pIndex t1 (Proxy::Proxy '[0,1,3]) @?= 8
case_index_21 = index t1 (0:-2:-0:-N) @?= 9
case_pIndex_21 = pIndex t1 (Proxy::Proxy '[0,2,0]) @?= 9
case_index_22 = index t1 (0:-2:-1:-N) @?= 10
case_pIndex_22 = pIndex t1 (Proxy::Proxy '[0,2,1]) @?= 10
case_index_23 = index t1 (0:-2:-2:-N) @?= 11
case_pIndex_23 = pIndex t1 (Proxy::Proxy '[0,2,2]) @?= 11
case_index_24 = index t1 (0:-2:-3:-N) @?= 12
case_pIndex_24 = pIndex t1 (Proxy::Proxy '[0,2,3]) @?= 12
case_index_25 = index t1 (1:-0:-0:-N) @?= 13
case_pIndex_25 = pIndex t1 (Proxy::Proxy '[1,0,0]) @?= 13
case_index_26 = index t1 (1:-0:-1:-N) @?= 14
case_pIndex_26 = pIndex t1 (Proxy::Proxy '[1,0,1]) @?= 14
case_index_27 = index t1 (1:-0:-2:-N) @?= 15
case_pIndex_27 = pIndex t1 (Proxy::Proxy '[1,0,2]) @?= 15
case_index_28 = index t1 (1:-0:-3:-N) @?= 16
case_pIndex_28 = pIndex t1 (Proxy::Proxy '[1,0,3]) @?= 16
case_index_29 = index t1 (1:-1:-0:-N) @?= 17
case_pIndex_29 = pIndex t1 (Proxy::Proxy '[1,1,0]) @?= 17
case_index_30 = index t1 (1:-1:-1:-N) @?= 18
case_pIndex_30 = pIndex t1 (Proxy::Proxy '[1,1,1]) @?= 18
case_index_31 = index t1 (1:-1:-2:-N) @?= 19
case_pIndex_31 = pIndex t1 (Proxy::Proxy '[1,1,2]) @?= 19
case_index_32 = index t1 (1:-1:-3:-N) @?= 20
case_pIndex_32 = pIndex t1 (Proxy::Proxy '[1,1,3]) @?= 20
case_index_33 = index t1 (1:-2:-0:-N) @?= 21
case_pIndex_33 = pIndex t1 (Proxy::Proxy '[1,2,0]) @?= 21
case_index_34 = index t1 (1:-2:-1:-N) @?= 22
case_pIndex_34 = pIndex t1 (Proxy::Proxy '[1,2,1]) @?= 22
case_index_35 = index t1 (1:-2:-2:-N) @?= 23
case_pIndex_35 = pIndex t1 (Proxy::Proxy '[1,2,2]) @?= 23
case_index_36 = index t1 (1:-2:-3:-N) @?= 24
case_pIndex_36 = pIndex t1 (Proxy::Proxy '[1,2,3]) @?= 24

case_generate_1 =
    let f :: SizedList 1 Int -> Int
        f (i:-N) = i
        v2 :: TensorBoxed '[4] Int
        v2 = generate f
    in
    case v2 of
        Tensor arr -> arr @?= fromList [0,1,2,3]

t2 :: TensorBoxed '[2,3,2] Int
t2 = [ [ [0, 1], [2, 3], [4, 5] ]
     , [ [6, 7], [8, 9], [10, 11] ]
     ]

case_contract_2 =
    contract t2 (Proxy::Proxy 0) (Proxy::Proxy 2) @?= [7, 11, 15]

t3 :: TensorBoxed '[2, 2, 2, 2] Int
t3 = [ [ [ [0,1]
         , [2,3]
         ]
       , [ [4,5]
         , [6,7]
         ]
       ]  
     , [ [ [8,9]
         , [10,11]
         ]
       , [ [12,13]
         , [14,15]
         ]
       ]
     ]

case_contract_3 =
    contract t3 (Proxy::Proxy 1) (Proxy::Proxy 3) @?= [[5,9],[21,25]]

cobTestM :: TensorBoxed '[2,2] Int
cobTestM = [[5,6],[7,8]]

cobM :: TensorBoxed '[2,2] Int
cobM = [[1,2],[3,4]]

case_changeBasis_1 =
    changeBasis cobTestM cobM (Proxy::Proxy '[0,1]) @?= [[63,145],[143,329]]

case_one_1 =
    one @?= ([[1]] :: TensorBoxed '[1,1] Int)

case_one_2 =
    one @?= ([[1,0], [0,1]] :: TensorBoxed '[2,2] Int)

case_one_3 =
    one @?= ([[1,0,0], [0,1,0], [0,0,1]] :: TensorBoxed '[3,3] Int)

newtype T0 = T0 (TensorBoxed '[] Int)
    deriving (Show, Eq)

instance (Monad m) => Serial m T0 where
    series = cons1 (T0 . tensor)

newtype T2 = T2 (TensorBoxed '[2] Int)
    deriving (Show, Eq)

instance (Monad m) => Serial m T2 where
    series = cons2 f
      where
        f :: Int -> Int -> T2
        f i j = T2 $ fromList [i, j]

newtype T22 = T22 (TensorBoxed '[2,2] Int)
    deriving (Show, Eq)

instance (Monad m) => Serial m T22 where
    series = cons4 f
      where
        f :: Int -> Int -> Int -> Int -> T22
        f i j k l = T22 $ fromList [[i, j], [k,l]]

smallCheckProperties = testGroup "SmallCheck"
    [ SC.testProperty "scalar . tensor" $
        \(i::Int) ->
            let t :: TensorBoxed '[] Int
                t = tensor i
            in
            i == scalar t
    , SC.testProperty "tensor . scalar" $
        \(T0 t) ->
            t == tensor (scalar t)
    , SC.testProperty "tensorProduct 1" $
        \(T2 t1, T2 t2) ->
            let prod = tensorProduct t1 t2
            in
            prod ==
                [ [ index t1 (0:-N) * index t2 (0:-N)
                  , index t1 (0:-N) * index t2 (1:-N)
                  ]
                , [ index t1 (1:-N) * index t2 (0:-N)
                  , index t1 (1:-N) * index t2 (1:-N)
                  ]
                ]
    , SC.testProperty "contract / trace" $
        \(T22 t) ->
            trace t == scalar (contract t (Proxy::Proxy 0) (Proxy::Proxy 1))
    , SC.testProperty "mul / dot" $
        \(T2 t1, T2 t2) ->
            dot t1 t2 == scalar (mul t1 (Proxy::Proxy 0) t2 (Proxy::Proxy 0))
    , SC.testProperty "changeBasisAll id" $
        \(T22 t) ->
            t == changeBasisAll t [[1,0],[0,1]]
    , SC.testProperty "changeBasisAll . changeBasisAll" $
        \(T22 t) ->
            t == changeBasisAll (changeBasisAll t [[2,5],[1,3]])
                [[3,-5],[-1,2]]
    , SC.testProperty "changeBasis id" $
        \(T2 t) ->
            t == changeBasis t [[1, 0], [0, 1]] (Proxy::Proxy '[0])
    , SC.testProperty "changeBasis 1" $
        \(T2 t) ->
            let m :: TensorBoxed '[2,2] Int
                m = [[3,5],[7,11]]
            in
            mul t (Proxy::Proxy 0) m (Proxy::Proxy 1)
            == changeBasis t m (Proxy::Proxy '[0])
    ]
