{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.Math.Ftensor.Lib.Array (
    tests
) where

import Prelude hiding (length, replicate)

import Control.Monad.ST
import GHC.Exts (IsList(..))
    
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.TH (testGroupGenerator)
import qualified Test.Tasty.SmallCheck as SC

import Math.Ftensor.Lib.Array

tests = testGroup "Tests.Math.Ftensor.Lib.Array" [smallCheckProperties, autoTests]

autoTests = $(testGroupGenerator)

array1 :: ArrayBoxed Int
array1 = runST $ do
    arr <- new 2
    write arr 0 100
    write arr 1 101
    freeze arr

case_length_1 = length array1 @?= 2

case_write_1 = index array1 0 @?= 100
case_write_2 = index array1 1 @?= 101

array2 :: ArrayPrim Int
array2 = runST $ do
    arr <- new 2
    write arr 0 100
    write arr 1 101
    freeze arr

case_length_2 = length array2 @?= 2

case_write_3 = index array2 0 @?= 100
case_write_4 = index array2 1 @?= 101

array3 :: ArrayBoxed Int
array3 = convert array2

case_convert_1 = length array3 @?= 2
case_convert_2 = index array3 0 @?= 100
case_convert_3 = index array3 1 @?= 101

array4 :: ArrayBoxed Int
array4 = convert array3

case_convert_4 = length array4 @?= 2
case_convert_5 = index array4 0 @?= 100
case_convert_6 = index array4 1 @?= 101

case_replicate_1 =
    let rep :: ArrayBoxed Int
        rep = runST $ replicate 5 13 >>= freeze
    in
    rep @?= [13,13,13,13,13]

case_replicate_2 =
    let rep :: ArrayPrim Int
        rep = runST $ replicate 5 13 >>= freeze
    in
    rep @?= [13,13,13,13,13]

smallCheckProperties = testGroup "SmallCheck"
    [ SC.testProperty "toList . fromList (Array)" $
        \(lst::[Int]) ->
            let (arr::ArrayBoxed Int) = fromList lst
            in
            lst == toList arr
    , SC.testProperty "toList . fromList (ArrayPrim)" $
        \(lst::[Int]) ->
            let (arr::ArrayPrim Int) = fromList lst
            in
            lst == toList arr
    ]
