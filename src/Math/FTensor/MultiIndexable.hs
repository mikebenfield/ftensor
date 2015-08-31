{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Math.FTensor.MultiIndexable (
) where

import Data.Proxy
import GHC.TypeLits

import qualified Data.Vector.Unboxed as U

data Nat_ = Ze | Su Nat_

class MultiIndexable t a (n::Nat_) where
    acc :: t -> U.Vector Int -> Int -> a

-- type family ToNat_ (n::Nat) :: Nat_ where
--     ToNat_ 0 = Ze
--     ToNat_ (m) = Su (ToNat_ (m-1))

instance MultiIndexable a a (Ze) where
    acc x _ _ = x

-- instance MultiIndexable [a] a 1 where
--     acc x vec i = x !! (vec U.! i)

instance forall b a m. (MultiIndexable b a m) => MultiIndexable [b] a (Su m) where
    acc x vec i =
        let k :: MultiIndexable b a n => b
            k = x !! (vec U.! i)
        in
        acc k vec (i+1)
