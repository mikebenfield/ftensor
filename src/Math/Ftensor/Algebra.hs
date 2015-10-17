{-|
Module: Math.Ftensor.Algebra
Copyright: (c) 2015 Michael Benfield
License: ISC

Classes supporting arithmetic operations. Haskell's numeric classes are too
coarse for our needs. Types which are instances of more than one of these
classes should satisfy the obvious compatibility relations between the various
operations.

For instance, the equalities @x -. x = zero@ and @x *. (y +. z) = x *. y +. x
*. z@ and @one *. x = x@ should hold. If the type in question implements
inexact arithmetic, they should hold approximately.

For an instance of @Num@, these should hold whenever the type is also an
instance of the classes defining these functions:

* @(+.) = (+)@

* @(*.) = (*)@

* @(-.) = (-)@

* @neg = negate@

* @one = fromInteger 1@

* @zero = fromInteger 0@

Similarly, for an instance of @Fractional@

* @(\/.) = (\/)@

* @inv = recip@
-}

{-# LANGUAGE CPP #-}

module Math.Ftensor.Algebra (
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

-- | Types that have an additive identity. Should satisfy:
--
-- * @x +. zero = zero +. x = x@
class Additive a => WithZero a where
    zero :: a

infixl 6 -.

-- | Types that have additive inverses. Should satisfy:
-- @neg x = zero -. x@ and @x -. x = zero@.
class WithZero a => WithNegatives a where
    {-# MINIMAL neg | (-.) #-}
    neg :: a -> a
    neg x = zero -. x
    (-.) :: a -> a -> a
    lhs -. rhs = lhs +. neg rhs

infixl 7 *.

class Multiplicative a where
    (*.) :: a -> a -> a

-- | Types with a multiplicative identity. Should satisfy:
--
-- * @one *. x = x *. one = x@.
class Multiplicative a => WithOne a where
    one :: a

infixl 7 /.

-- | Types with multiplicative inverse.
-- @inv x@ and @y /. x@
-- may throw an exception or behave in undefined ways if
-- @invertible x = False@.
--
-- * @inv x = one ./ x@ 
--
-- * @y *. x /. y = x@ 
--
-- * @x /. y *. x = y@
class WithOne a => WithReciprocals a where

    {-# MINIMAL invertible, (inv | (/.)) #-}
    invertible :: a -> Bool

    inv :: a -> a
    inv x = one /. x

    (/.) :: a -> a -> a
    lhs /. rhs = lhs *. inv rhs

infixl 7 *:

-- | Types like mathematical vectors that can be multiplied by another type.
class WithScalars a where
    type Scalar a
    (*:) :: Scalar a -> a -> a

-- | A @Rg@ is a Ring without one or negatives.
type Rg a = (WithZero a, Multiplicative a)

-- | A @Rng@ is a Ring without one.
type Rng a = (Rg a, WithNegatives a)

-- | A @Rig@ is a Ring without negatives.
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
; {-# INLINE invertible #-} \
; invertible = (/= 0) \
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
