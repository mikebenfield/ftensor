{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}

import Data.Proxy
import GHC.TypeLits
import GHC.Exts (fromList)

import Criterion.Main

import Math.FTensor.General
import Math.FTensor.Lib.General
import Math.FTensor.SizedList

u333 :: TensorPrim '[3,3,3] Int
u333 = fromList
    [[[1, 2, 3],[4,5,6],[7,8,9]],
     [[10,11,12],[13,14,15],[16,17,18]],
     [[19,20,21],[22,23,24],[25,26,27]]]

v333 :: TensorBoxed '[3,3,3] Int
v333 = convert u333

uBig :: TensorPrim '[2,2,2,2,2,2,2,2,2,2] Int
uBig = Tensor (fromList [1..2^(10::Int)])

-- vBig :: TensorV Int 2
-- vBig = convert uBig

main = defaultMain
    [ bgroup "tensorProduct"
        [ bench "index" $ nf (index u333) (2:-1:-2:-N)
        , bench "unsafeIndex" $ nf (unsafeIndex u333) (2:-1:-2:-N)
        , bench "pIndex" $ nf (pIndex u333) (Proxy::Proxy '[2,1,2])
        , bench "index (boxed)" $ nf (index v333) (2:-1:-2:-N)
        , bench "unsafeIndex (boxed)" $ nf (unsafeIndex v333) (2:-1:-2:-N)
        , bench "pIndex (boxed)" $ nf (pIndex v333) (Proxy::Proxy '[2,1,2])
        , bench "index uBig" $ nf (index uBig)
            (0:-1:-0:-1:-0:-1:-0:-1:-0:-1:-N)
        , bench "unsafeIndex uBig" $ nf (unsafeIndex uBig)
            (0:-1:-0:-1:-0:-1:-0:-1:-0:-1:-N)
        , bench "pIndex uBig" $ nf (pIndex uBig)
            (Proxy::Proxy '[0,1,0,1,0,1,0,1,0,1])
        -- , bench "u0 u0 2" $ nf (tensorProduct2 u0) u0
        -- , bench "u0 u0 3" $ nf (tensorProduct3 u0) u0
        -- , bench "v0 v0 1" $ nf (tensorProduct v0) v0
        -- , bench "v0 v0 2" $ nf (tensorProduct2 v0) v0
        -- , bench "v0 v0 3" $ nf (tensorProduct3 v0) v0
        -- , bench "u333 u0 1" $ nf (tensorProduct u333) u0
        -- , bench "u333 u0 2" $ nf (tensorProduct2 u333) u0
        -- , bench "u333 u0 3" $ nf (tensorProduct3 u333) u0
        -- , bench "v333 v0 1" $ nf (tensorProduct v333) v0
        -- , bench "v333 v0 2" $ nf (tensorProduct2 v333) v0
        -- , bench "v333 v0 3" $ nf (tensorProduct3 v333) v0
        -- , bench "u333 u333 1" $ nf (tensorProduct u333) u333
        -- , bench "u333 u333 2" $ nf (tensorProduct2 u333) u333
        -- , bench "u333 u333 3" $ nf (tensorProduct3 u333) u333
        -- , bench "v333 v333 1" $ nf (tensorProduct v333) v333
        -- , bench "v333 v333 2" $ nf (tensorProduct2 v333) v333
        -- , bench "v333 v333 3" $ nf (tensorProduct3 v333) v333
        -- , bench "uBig uBig 1" $ nf (tensorProduct uBig) uBig
        -- , bench "uBig uBig 2" $ nf (tensorProduct2 uBig) uBig
        -- , bench "uBig uBig 3" $ nf (tensorProduct3 uBig) uBig
        -- , bench "vBig vBig 1" $ nf (tensorProduct vBig) vBig
        -- , bench "vBig vBig 2" $ nf (tensorProduct2 vBig) vBig
        -- , bench "vBig vBig 3" $ nf (tensorProduct3 vBig) vBig
        ]
    ]

