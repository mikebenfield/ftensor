{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Tests.Math.FTensor.Core (
    tests
) where

import Prelude hiding (head)
import Data.Proxy

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck hiding (generate, scale)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.TH (testGroupGenerator)
import qualified Test.Tasty.SmallCheck as SC

import Test.SmallCheck.Series hiding (generate)

import qualified Data.Vector.Generic as G
import qualified Data.Vector as V
import Data.Functor.Identity (Identity)

import Math.FTensor.Core
import Math.FTensor.Simple

tests = testGroup "Tests.Math.FTensor.Core" [smallCheckProperties, autoTests]

autoTests = $(testGroupGenerator)

case_validShape_1 = validShape (Proxy::Proxy 0) [1] @?= False

case_validShape_2 = validShape (Proxy::Proxy 0) [] @?= True

case_validShape_3 = validShape (Proxy::Proxy 1) [] @?= False

case_validShape_4 = validShape (Proxy::Proxy 1) [1] @?= True

case_validShape_5 = validShape (Proxy::Proxy 2) [3, 4] @?= True

case_validShape_6 = validShape (Proxy::Proxy 2) [3, 0] @?= False

case_inBounds_1 = inBounds [3, 4, 5] [2, 3, 5] @?= False

case_inBounds_2 = inBounds [3, 4, 5] [2, 3, 1] @?= True

case_inBounds_3 = inBounds [] [] @?= True

case_inBounds_4 = inBounds [] [1] @?= False

case_inBounds_5 = inBounds [3, 4] [2, -1] @?= False

newtype ScalarT a = ScalarT (SimpleTensor a 0)
    deriving Show

newtype Vec1 a = Vec1 (SimpleTensor a 1)
    deriving Show
newtype Vec2 a = Vec2 (SimpleTensor a 1)
    deriving Show
newtype Vec3 a = Vec3 (SimpleTensor a 1)
    deriving Show
newtype Vec4 a = Vec4 (SimpleTensor a 1)
    deriving Show

newtype Mat11 a = Mat11 (SimpleTensor a 2)
    deriving Show
newtype Mat22 a = Mat22 (SimpleTensor a 2)
    deriving Show

instance (Serial m a) => Serial m (ScalarT a) where
    series = cons1 f
      where
        f i = ScalarT $ generate [] (\_ -> i)

instance (Serial m a) => Serial m (Vec1 a) where
    series = cons1 f
      where
        f i = Vec1 $ generate [1] (\_ -> i)

instance (Serial m a) => Serial m (Vec2 a) where
    series = cons2 f
      where
        f :: a -> a -> Vec2 a
        f i j = Vec2 $ generate [2] (([i, j] !!) . G.head)

instance (Serial m a) => Serial m (Vec3 a) where
    series = cons3 f
      where
        f :: a -> a -> a -> Vec3 a
        f i j k = Vec3 $ generate [3] (([i,j,k] !!) . G.head)

instance (Serial m a) => Serial m (Vec4 a) where
    series = cons4 f
      where
        f :: a -> a -> a -> a -> Vec4 a
        f i j k l = Vec4 $ generate [4] (([i,j,k,l] !!) . G.head)

instance (Serial m a) => Serial m (Mat11 a) where
    series = cons1 f
      where
        f :: a -> Mat11 a
        f i = Mat11 $ generate [1, 1] (\_ -> i)

instance (Serial m a) => Serial m (Mat22 a) where
    series = cons4 f
      where
        f :: a -> a -> a -> a -> Mat22 a
        f i j k l = Mat22 $ generate [2, 2] f
          where
            f (G.toList -> [0, 0]) = i
            f (G.toList -> [0, 1]) = j
            f (G.toList -> [1, 0]) = k
            f _ = i
            -- (\vec -> case (vec G.! 0, vec G.! 1) of
            --     (0, 0) -> i
            --     (0, 1) -> j
            --     (1, 0) -> k
            --     _ -> l)

smallCheckProperties = testGroup "SmallCheck"
    [ SC.testProperty "scale 1" $
        \(Vec1 v) -> 
            let scaled :: SimpleTensor Int 1
                scaled = scale v 3
            in
            3 * index v [0] == index scaled [0]

    , SC.testProperty "scale 2" $
        \(Vec2 v) -> 
            let scaled :: SimpleTensor Int 1
                scaled = scale v 5
            in
            5 * index v [0] == index scaled [0] &&
            5 * index v [1] == index scaled [1]

    , SC.testProperty "scale 3" $
        \(Mat11 v) -> 
            let scaled :: SimpleTensor Int 2
                scaled = scale v 7
            in
            7 * index v [0, 0] == index scaled [0, 0]

    , SC.testProperty "add 1" $
        \((Vec1 v, Vec1 w)) -> 
            let sum :: SimpleTensor Int 1
                sum = add v w
            in
            index sum [0] == index v [0] + index w [0]

    , SC.testProperty "add 2" $
        \(Vec2 v, Vec2 w) -> 
            let sum :: SimpleTensor Int 1
                sum = add v w
            in
            index sum [0] == index v [0] + index w [0] &&
            index sum [1] == index v [1] + index w [1]

    , SC.testProperty "add 3" $
        \(ScalarT v, ScalarT w) -> 
            let sum :: SimpleTensor Int 0
                sum = add v w
            in
            index sum [] == index v [] + index w []
            
    , SC.testProperty "tensorMultiply 1" $
        \(Vec1 v, Vec1 w) ->
            let product :: SimpleTensor Int 2
                product = tensorMultiply v w
            in
            shape product == [1, 1] &&
            index product [0, 0] == index v [0] * index w [0]

    , SC.testProperty "tensorMultiply 2" $
        \(Vec2 v, Vec2 w) ->
            let product :: SimpleTensor Int 2
                product = tensorMultiply v w
            in
            shape product == [2, 2] &&
            index product [0, 0] == index v [0] * index w [0] &&
            index product [0, 1] == index v [0] * index w [1] &&
            index product [1, 0] == index v [1] * index w [0] &&
            index product [1, 1] == index v [1] * index w [1]

    , SC.testProperty "tensorMultiply 3" $
        \(Mat11 v, Vec1 w) ->
            let product :: SimpleTensor Int 3
                product = tensorMultiply v w
            in
            shape product == [1, 1, 1] &&
            index product [0, 0, 0] == index v [0, 0] * index w [0]

    , SC.testProperty "tensorMultiply 4" $
        \(Vec1 v, Vec2 w) ->
            let product :: SimpleTensor Int 2
                product = tensorMultiply v w
            in
            shape product == [1, 2] &&
            index product [0, 0] == index v [0] * index w [0] &&
            index product [0, 1] == index v [0] * index w [1]

    , SC.testProperty "tensorMultiply 5" $
        \(ScalarT v, Vec2 w) ->
            let product :: SimpleTensor Int 1
                product = tensorMultiply v w
            in
            shape product == [2] &&
            index product [0] == index v [] * index w [0] &&
            index product [1] == index v [] * index w [1]

    , SC.testProperty "contract 1" $
        \(Mat22 v) ->
            let c :: SimpleTensor Int 0
                c = contract v 0 1
            in
            shape c == [] &&
            index c [] == index v [0, 0] + index v [1, 1]

    ]
