{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.Math.FTensor.InternalArray (
    tests
) where

import Control.Monad.ST
import Prelude hiding (length)
    
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.TH (testGroupGenerator)

import qualified Data.Vector.Generic as G
import qualified Data.Vector as V

import Math.FTensor.InternalArray

tests = testGroup "Tests.Math.FTensor.InternalArray" [smallCheckProperties, autoTests]

autoTests = $(testGroupGenerator)

array1 :: Array Int
array1 = runST $ do
    arr <- new 2
    write arr 0 100
    write arr 1 101
    freeze arr

case_length_1 = length array1 @?= 2

case_write_1 = index array1 0 @?= 100
case_write_2 = index array1 1 @?= 101

array2 :: PrimArray Int
array2 = runST $ do
    arr <- new 2
    write arr 0 100
    write arr 1 101
    freeze arr

case_length_2 = length array2 @?= 2

case_write_3 = index array2 0 @?= 100
case_write_4 = index array2 1 @?= 101

smallCheckProperties = testGroup "SmallCheck" []
