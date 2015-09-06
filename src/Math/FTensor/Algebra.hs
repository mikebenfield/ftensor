
module Math.FTensor.Algebra (
) where

class Num (Scalar vs) => VectorSpace vs where
    type Scalar vs
    (+.) :: vs -> vs -> vs
    (*:) :: Scalar vs -> vs -> vs

class VectorSpace a => Algebra a where
    (*.) :: a -> a -> a

instance Num a => VectorSpace a where
    type Scalar a = a
    (+.) = (+)
    (*:) = (*)

instance Num a => Algebra a where
    (*.) = (*)
