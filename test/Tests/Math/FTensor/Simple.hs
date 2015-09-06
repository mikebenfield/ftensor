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

import GHC.Exts (IsList(..))

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)
import Test.Tasty.TH (testGroupGenerator)
import qualified Test.Tasty.SmallCheck as SC

import Test.SmallCheck.Series hiding (generate, Positive)

import Math.FTensor.Lib.Simple
import Math.FTensor.Simple
import Math.FTensor.SizedList

autoTests = $(testGroupGenerator)

tests = testGroup "tests" [autoTests, smallCheckProperties]

v1 :: TensorBoxed 3 1 Int
v1 = [1,2,3]

case_index_1 = index v1 (0:-N) @?= 1
case_index_2 = index v1 (1:-N) @?= 2
case_index_3 = index v1 (2:-N) @?= 3

m1 :: TensorBoxed 3 2 Int
m1 = [[1,2,3], [4,5,6], [7,8,9]]

case_index_4 = index m1 (0:-0:-N) @?= 1
case_index_5 = index m1 (1:-1:-N) @?= 5
case_index_6 = index m1 (1:-2:-N) @?= 6
case_index_7 = index m1 (2:-1:-N) @?= 8
case_index_8 = index m1 (2:-2:-N) @?= 9

t3 :: TensorBoxed 2 3 Int
t3 = [[[1,2], [3,4]], [[5,6], [7,8]]]

case_index_9 = index t3 (0:-0:-0:-N) @?= 1
case_index_10 = index t3 (0:-0:-1:-N) @?= 2
case_index_11 = index t3 (0:-1:-0:-N) @?= 3
case_index_12 = index t3 (0:-1:-1:-N) @?= 4
case_index_13 = index t3 (1:-0:-0:-N) @?= 5
case_index_14 = index t3 (1:-0:-1:-N) @?= 6
case_index_15 = index t3 (1:-1:-0:-N) @?= 7
case_index_16 = index t3 (1:-1:-1:-N) @?= 8

m1' :: TensorPrim 3 2 Int
m1' = convert m1

case_convert_1 :: Assertion
case_convert_1 = case contents m1' of
    Positive arr -> toList arr @?= [1,2,3,4,5,6,7,8,9]

smallCheckProperties = testGroup "SmallCheck" []
