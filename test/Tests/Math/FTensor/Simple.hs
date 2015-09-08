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

smallCheckProperties = testGroup "SmallCheck" []
