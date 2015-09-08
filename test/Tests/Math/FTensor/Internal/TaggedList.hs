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

list1 :: TaggedList (3 ': '[]) Int
list1 = fromList [0, 1, 2]

case_toList_1 = toList list1 @?= [0,1,2]

smallCheckProperties = testGroup "SmallCheck" []
