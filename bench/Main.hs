{-# LANGUAGE DataKinds #-}

import GHC.TypeLits
import GHC.Exts (fromList)

import Criterion.Main

import Math.FTensor.Core2

u0 :: TensorU Int 0
u0 = tensor 7

v0 :: TensorV Int 0
v0 = convert u0

u333 :: TensorU Int 3
u333 = fromList
    [[[1, 2, 3],[4,5,6],[7,8,9]],
     [[10,11,12],[13,14,15],[16,17,18]],
     [[19,20,21],[22,23,24],[25,26,27]]]

v333 :: TensorV Int 3
v333 = convert u333

uBig :: TensorU Int 2
uBig = fromList
    [[1..500],
     [1..500],
     [1..500]]

vBig :: TensorV Int 2
vBig = convert uBig

main = defaultMain
    [ bgroup "tensorProduct"
        [ bench "u0 u0 1" $ nf (tensorProduct u0) u0
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

