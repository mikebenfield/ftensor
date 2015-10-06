{-# LANGUAGE CPP #-}

module Math.FTensor.Algebra (
    Additive(..),
    WithZero(..),
    WithNegatives(..),
    Multiplicative(..),
    WithOne(..),
    WithReciprocals(..),
    WithScalars(..),
    Rg,
    Rng,
    Rig,
    Ring,
    Field,
    Module,
    VectorSpace,
    Algebra,
    UnitalAlgebra,
) where

import Data.Complex (Complex)
import Data.Int
import Data.Ratio (Ratio)
import Data.Word
import Numeric.Natural

infixl 6 +.

class Additive a where
    (+.) :: a -> a -> a

class Additive a => WithZero a where
    zero :: a

infixl 6 -.

class WithZero a => WithNegatives a where
    neg :: a -> a
    (-.) :: a -> a -> a

infixl 7 *.

class Multiplicative a where
    (*.) :: a -> a -> a

class Multiplicative a => WithOne a where
    one :: a

infixl 6 /.

class WithOne a => WithReciprocals a where
    inv :: a -> a
    (/.) :: a -> a -> a

infixl 7 *:

class WithScalars a where
    type Scalar a
    (*:) :: Scalar a -> a -> a

type Rg a = (WithZero a, Multiplicative a)

type Rng a = (Rg a, WithNegatives a)

type Rig a = (Rg a, WithOne a)

type Ring a = (Rng a, Rig a)

type Field a = (Ring a, WithReciprocals a)

type Module a = (WithNegatives a, WithScalars a, Ring (Scalar a))

type VectorSpace a = (Module a, Field (Scalar a))

type Algebra a = (VectorSpace a, Rng a)

type UnitalAlgebra a = (Algebra a, WithOne a)

#define INSTANCES_NUM(typ, ctxt) \
instance ctxt Additive (typ) where { \
; {-# INLINE (+.) #-} \
; (+.) = (+) \
} ; instance ctxt WithZero (typ) where { \
; {-# INLINE zero #-} \
; zero = 0 \
} ; instance ctxt WithNegatives (typ) where { \
; {-# INLINE neg #-} \
; neg = negate \
; {-# INLINE (-.) #-} \
; (-.) = (-) \
} ; instance ctxt Multiplicative (typ) where { \
; {-# INLINE (*.) #-} \
; (*.) = (*) \
} ; instance ctxt WithOne (typ) where { \
; {-# INLINE one #-} \
; one = 1 \
} ; instance ctxt WithScalars (typ) where { \
; type Scalar (typ) = typ \
; {-# INLINE (*:) #-} \
; (*:) = (*) \
}

#define INSTANCES_FRACTIONAL(typ, ctxt) \
INSTANCES_NUM(typ, ctxt) ; \
instance ctxt WithReciprocals (typ) where { \
; {-# INLINE inv #-} \
; inv = recip \
; {-# INLINE (/.) #-} \
; (/.) = (/) \
}

INSTANCES_NUM(Natural, )
INSTANCES_NUM(Integer, )
INSTANCES_NUM(Int, )
INSTANCES_NUM(Int8, )
INSTANCES_NUM(Int16, )
INSTANCES_NUM(Int32, )
INSTANCES_NUM(Int64, )
INSTANCES_NUM(Word, )
INSTANCES_NUM(Word8, )
INSTANCES_NUM(Word16, )
INSTANCES_NUM(Word32, )
INSTANCES_NUM(Word64, )

INSTANCES_FRACTIONAL(Float, )
INSTANCES_FRACTIONAL(Double, )
INSTANCES_FRACTIONAL(Complex a, RealFloat a =>)
INSTANCES_FRACTIONAL(Ratio a, Integral a =>)
