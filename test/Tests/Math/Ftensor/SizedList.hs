{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Tests.Math.Ftensor.SizedList (
    tests
) where

import Data.Proxy
import GHC.Exts

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.TH (testGroupGenerator)

import Math.Ftensor.SizedList

tests = testGroup "Tests.Math.Ftensor.SizedList"
    [smallCheckProperties, autoTests]

autoTests = $(testGroupGenerator)

case_foldr_1 =
    foldr (-) 0 ((1::Int):-2:-3:-N) @?= 2

case_foldr_2 =
    foldr (flip (-)) 0 ((1::Int):-2:-3:-N) @?= (-6)

case_foldr_3 =
    foldr (Prelude.++) [] ([0::Int,1]:-[2,3]:-N) @?= [0,1,2,3]

case_foldr_4 =
    foldr (:) [] ([0::Int,1]:-[2,3]:-N) @?= [[0,1],[2,3]]

smallCheckProperties = testGroup "SmallCheck" []
