{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.Math.FTensor.Internal.TaggedList (
    tests
) where

import GHC.Exts (IsList(..))
import GHC.TypeLits
    
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.TH (testGroupGenerator)

import Math.FTensor.Internal.TaggedList

tests = testGroup "Tests.Math.FTensor.Internal.TaggedList"
    [smallCheckProperties, autoTests]

autoTests = $(testGroupGenerator)

list1 :: TaggedList 3 'Z Int
list1 = fromList [0, 1, 2]

case_toList_1 = toList list1 @?= [0, 1, 2]

list2 :: TaggedList 2 (NatToNat_ 1) Int
list2 = fromList [fromList [0, 1], fromList [2, 3]]

case_foldr_1 = foldr (:) [] list2 @?= [0,1,2,3]

list3 :: TaggedList2 (3 ': '[]) Int
list3 = fromList [0, 1, 2]

case_toList_2 = toList list3 @?= [0,1,2]


smallCheckProperties = testGroup "SmallCheck" []
