{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Tests.Math.FTensor.Core2 (
    tests
) where

import Data.Proxy
import GHC.Exts

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck hiding (generate, scale)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.TH (testGroupGenerator)
import qualified Test.Tasty.SmallCheck as SC

import Test.SmallCheck.Series hiding (generate)

import qualified Data.Vector.Generic as G
import qualified Data.Vector as V
import Data.Functor.Identity (Identity)

import Math.FTensor.Core2
import Math.FTensor.SizedList

tests = testGroup "Tests.Math.FTensor.Core2" [smallCheckProperties, autoTests]

autoTests = $(testGroupGenerator)

m1 :: TensorV Int 2
m1 = fromList [[1, 2], [3, 4]]

case_contract_1 = contract m1 0 1 @?= tensor 5

m2 :: TensorV Int 2
m2 = fromList [[1,2,3], [4,5,6], [7,8,9]]

case_contract_2 = contract m2 0 1 @?= tensor 15

m3 :: TensorV Int 3
m3 = fromList [[[1,2], [3,4]], [[5,6],[7,8]]]

case_contract_3 = contract m3 1 2 @?= fromList [5, 13]
case_contract_4 = contract m3 0 1 @?= fromList [8, 10]
case_contract_5 = contract m3 0 2 @?= fromList [7, 11]

newtype T0 = T0 (TensorV Int 0)
    deriving (Show, Eq)

instance (Monad m) => Serial m T0 where
    series = cons1 (T0 . tensor)

newtype T3 = T3 (TensorV Int 1)
    deriving (Show, Eq)

instance (Monad m) => Serial m T3 where
    series = cons3 f
      where
        f :: Int -> Int -> Int -> T3
        f i j k = T3 $ fromList [i, j, k]

newtype T2 = T2 (TensorV Int 1)
    deriving (Show, Eq)

instance (Monad m) => Serial m T2 where
    series = cons2 f
      where
        f :: Int -> Int -> T2
        f i j = T2 $ fromList [i, j]

newtype T22 = T22 (TensorV Int 2)
    deriving (Show, Eq)

instance (Monad m) => Serial m T22 where
    series = cons4 f
      where
        f :: Int -> Int -> Int -> Int -> T22
        f i j k l = T22 $ fromList [[i, j], [k,l]]

newtype T311 = T311 (TensorV Int 3)
    deriving (Show, Eq)

instance (Monad m) => Serial m T311 where
    series = cons3 f
      where
        f :: Int -> Int -> Int -> T311
        f i j k = T311 $ fromList [[[i]], [[j]], [[k]]]

smallCheckProperties = testGroup "SmallCheck"
    [ SC.testProperty "tensor . scalar" $
        \(T0 t) -> t == (tensor . scalar) t
    , SC.testProperty "fromList . toList 1" $
        \(T3 t) -> t == (fromList . toList) t
    , SC.testProperty "fromList . toList 2" $
        \(T311 t) -> t == (fromList . toList) t
    , SC.testProperty "tensorProduct scaling 1" $
        \(T0 t1, T0 t2) ->
            tensorProduct t1 t2 == tensor (scalar t1 * scalar t2)
    , SC.testProperty "tensorProduct scaling 2" $
        \(T0 t1, T311 t2) ->
            let s = scalar t1
                idx i = index t2 (i:-0:-0:-N)
            in
            tensorProduct t1 t2 ==
                fromList
                    [ [[s*idx 0]]
                    , [[s*idx 1]]
                    , [[s*idx 2]]
                    ]
    , SC.testProperty "tensorProduct 1" $
        \(T2 t1, T2 t2) ->
            let f i j = index t1 (i:-N) * index t2 (j:-N)
            in
            tensorProduct t1 t2 == fromList
                [ [f 0 0, f 0 1]
                , [f 1 0, f 1 1]
                ]
     ]
