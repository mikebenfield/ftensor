{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}

import Data.Proxy
import GHC.TypeLits
import GHC.Exts (fromList)

import Criterion.Main

import Math.FTensor.General
import Math.FTensor.Lib.General
import Math.FTensor.SizedList
import qualified Math.FTensor.Lib.Array as A

u333 :: TensorPrim '[3,3,3] Int
u333 = fromList
    [[[1, 2, 3],[4,5,6],[7,8,9]],
     [[10,11,12],[13,14,15],[16,17,18]],
     [[19,20,21],[22,23,24],[25,26,27]]]

v333 :: TensorBoxed '[3,3,3] Int
v333 = convert u333

uBig :: TensorPrim '[2,2,2,2,2,2,2,2,2,2] Int
uBig = Tensor (fromList [1..2^(10::Int)])

vMat :: TensorBoxed '[10,10] Int
vMat = Tensor (fromList [1..100])

uMat :: TensorPrim '[10,10] Int
uMat = convert vMat

cont :: TensorPrim '[10, 10] Int -> TensorPrim '[] Int
cont t = contract t (Proxy::Proxy 0) (Proxy:: Proxy 1)

pInd :: TensorPrim '[3,3,3] Int -> Int
pInd t = pIndex t (Proxy::Proxy '[2,1,2])

main = defaultMain
    [ bgroup "trace"
        [ bench "trace uMat" $ nf trace uMat
        , bench "trace vMat" $ nf trace vMat
        ]
    , bgroup "contract"
        [ bench "contract uMat" $ nf cont uMat
        ]
    , bgroup "indexing"
        [ bench "index" $ nf (index u333) (2:-1:-2:-N)
        , bench "unsafeIndex" $ nf (unsafeIndex u333) (2:-1:-2:-N)
        , bench "pIndex" $ nf pInd u333
        , bench "index (boxed)" $ nf (index v333) (2:-1:-2:-N)
        , bench "unsafeIndex (boxed)" $ nf (unsafeIndex v333) (2:-1:-2:-N)
        , bench "pIndex (boxed)" $ nf (pIndex v333) (Proxy::Proxy '[2,1,2])
        , bench "index uBig" $ nf (index uBig)
            (0:-1:-0:-1:-0:-1:-0:-1:-0:-1:-N)
        , bench "unsafeIndex uBig" $ nf (unsafeIndex uBig)
            (0:-1:-0:-1:-0:-1:-0:-1:-0:-1:-N)
        , bench "pIndex uBig" $ nf (pIndex uBig)
            (Proxy::Proxy '[0,1,0,1,0,1,0,1,0,1])
        ]
    ]

