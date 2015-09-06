{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.Math.FTensor.InternalTaggedList (
    tests
) where

import GHC.Exts (IsList(..))
    
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.TH (testGroupGenerator)

import Math.FTensor.InternalTaggedList

tests = testGroup "Tests.Math.FTensor.InternalTaggedList"
    [smallCheckProperties, autoTests]

autoTests = $(testGroupGenerator)

list1 :: TaggedList 3 'Z Int
list1 = fromList [0, 1, 2]

case_toList_1 = toList list1 @?= [0, 1, 2]

list2 :: TaggedList 2 (NatToNat_ 1) Int
list2 = fromList [fromList [0, 1], fromList [2, 3]]

case_foldr_1 = foldr (:) [] list2 @?= [0,1,2,3]

smallCheckProperties = testGroup "SmallCheck" []
