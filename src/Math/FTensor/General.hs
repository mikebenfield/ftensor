{-|
Module: Math.FTensor.General
Copyright: (c) 2015 Michael Benfield
License: BSD-3

General tensor types.
-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-} -- for ContractedDims, IsList

module Math.FTensor.General (
    -- * Types
    Tensor(..),
    TensorBoxed,
    TensorPrim,
    MultiIndex,

    -- * Indexing
    pIndex,
    unsafeIndex,
    index,
    maybeIndex,
    scalar,

    -- * Creating
    generate,
    convert,
    tensor,

    -- * Mathematical operations
    scale,
    add,
    minus,
    tensorProduct,
    contract,
    mul,
    trace,
    dot,
    changeBasis,
    changeBasisAll,

    -- * Constraints
    -- | Part of the implementation for these algorithms happens at the type
    -- level, and that is contained in these constraints. For many users of
    -- this library, this may be considered an implementation detail. But some
    -- users writing general functions may have need of these constraints.

    PIndexConstraint,
    GenerateConstraint,
    TensorProductConstraint,
    ContractConstraint,
    MulConstraint,
    ChangeBasisConstraint,
    ContractedDims,
) where

import Control.Monad (replicateM)
import Control.Monad.ST (runST, ST)
import Data.Foldable (traverse_)
import Data.Proxy
import Data.STRef
import GHC.Exts (Constraint, IsList(..))
import GHC.TypeLits

import Control.DeepSeq

import qualified Math.FTensor.Internal.Check

import Math.FTensor.Lib.General
import Math.FTensor.Lib.TypeList
import qualified Math.FTensor.Lib.Array as A

import Math.FTensor.Algebra
import Math.FTensor.SizedList

#include "ftensor.h"

-- * Types

newtype Tensor a (dims::[Nat]) e = Tensor (a e)
  deriving (Eq, Functor, Traversable, Foldable)

instance (Show e, A.Array a e) => Show (Tensor a '[] e) where
    showsPrec d (Tensor arr) =
        showParen (d >= 10) $ showString "tensor " . shows (A.index arr 0)

instance (Show (ItemType (d ': ds) e), IsList (Tensor a (d ': ds) e))
    => Show (Tensor a (d ': ds) e) where

    showsPrec i = showsPrec i . toList

instance NFData (a e) => NFData (Tensor a dims e) where
    rnf (Tensor arr) = rnf arr

-- | This class exists only to implement @IsList@ for @Tensor@
class BuilderConsumer t e (lengths::[Nat]) | e lengths -> t where

    -- | @consume@ is really the same idea as @traverse@, but I want to apply
    -- it to a nested list.
    consume :: Applicative a => Proxy lengths -> (e -> a ()) -> t -> a ()

    build :: Monad m => Proxy lengths -> m e -> m t

instance BuilderConsumer e e '[] where
    {-# INLINE consume #-}
    consume _ = \f e -> f e

    {-# INLINE build #-}
    build _ = id

instance (BuilderConsumer contained e lengths, KnownNat length)
    => BuilderConsumer [contained] e (length ': lengths) where

    {-# INLINE consume #-}
    consume _ = \f list ->
        UNSAFE_CHECK
            (length' == Prelude.length list)
            "fromList"
            (length', Prelude.length list)
            $ traverse_ (consume (Proxy::Proxy lengths) f) list
      where
        length' :: Int
        length' = summon (Proxy::Proxy length)

    {-# INLINE build #-}
    build _ f = replicateM length (build (Proxy::Proxy lengths) f)
      where
        length :: Int
        length = summon (Proxy::Proxy length)

type family ItemType (lengths::[Nat]) e where
    ItemType '[len] e = e
    ItemType (len ': lens) e = [ItemType lens e]

instance
    ( A.Array a e
    , BuilderConsumer [ItemType dims e] e dims
    , KnownNat (Product dims)
    )
    => IsList (Tensor a dims e) where

    type Item (Tensor a dims e) = ItemType dims e

    fromList list =
        Tensor $ runST $ do
            newArr <- A.new $ summon (Proxy::Proxy (Product dims))
            writer <- A.writer newArr
            consume (Proxy::Proxy dims) writer list
            A.freeze newArr

    fromListN _ = fromList

    toList (Tensor arr) = runST $ do
        idx <- newSTRef (0::Int)
        let f = do
                i <- readSTRef idx
                writeSTRef idx (i+1)
                return $ A.index arr i
        build (Proxy::Proxy dims) f

type TensorBoxed = Tensor A.ArrayBoxed

type TensorPrim = Tensor A.ArrayPrim

type MultiIndex (dims::[Nat]) = SizedList (Length dims) Int

-- * Indexing

type PIndexConstraint a e (dims::[Nat]) (multiIndex::[Nat]) =
    ( A.Array a e
    , InBounds dims multiIndex
    , KnownNat (MultiIndexToI dims multiIndex)
    )

-- | Pick a component of a tensor via a type level multi index. May be more
-- efficient than @unsafeIndex@, @index@, or @maybeIndex@ because the offset
-- into the underlying array is computed at compile time.
--
-- >>> let (x::TensorBoxed '[2, 2] Int) = fromList [[0, 1], [2, 3]]
-- >>> pIndex x (Proxy::Proxy '[1, 0])
-- 2
pIndex
    :: forall a e (dims::[Nat]) (multiIndex::[Nat]).
    PIndexConstraint a e dims multiIndex
    => Tensor a dims e
    -> Proxy multiIndex
    -> e
pIndex (Tensor arr) p = A.index arr (multiIndexToI' (Proxy::Proxy dims) p)

{-# INLINE pIndex #-}

-- | Unsafely access a component of a tensor via a term level multi index. Will
-- perform bounds checks when the package is compiled with the Cabal option
-- -fUnsafeChecks (off by default). Example (assuming @N@ and @(:-)@ from
-- @Math.FTensor.SizedList@ are in scope):
--
-- >>> let (x::TensorBoxed '[2, 2] Int) = fromList [[0, 1], [2, 3]]
-- >>> unsafeIndex x (1:-0:-N)
-- 2
unsafeIndex
    :: forall a e (dims::[Nat]).
    ( A.Array a e
    , KnownType dims (MultiIndex dims)
    )
    => Tensor a dims e
    -> MultiIndex dims
    -> e
unsafeIndex (Tensor arr) multiIndex =
    UNSAFE_CHECK
        (inBounds p multiIndex)
        "unsafeIndex"
        ((summon p)::MultiIndex dims, multiIndex)
        $ A.index arr (multiIndexToI p multiIndex)
    where
      p :: Proxy dims
      p = Proxy

{-# INLINE unsafeIndex #-}

-- | Access a component of a tensor via a term level multi index. Will perform
-- bounds checks when the package is compiled with the Cabal option
-- -fBoundsChecks (on by default). Example (assuming @N@ and @(:-)@ from
-- @Math.FTensor.SizedList@ are in scope):
--
-- >>> let (x::TensorBoxed '[2, 2] Int) = fromList [[0, 1], [2, 3]]
-- >>> index x (1:-0:-N)
-- 2
index
    :: forall a e (dims::[Nat]).
    ( A.Array a e
    , KnownType dims (MultiIndex dims)
    )
    => Tensor a dims e
    -> MultiIndex dims
    -> e
index t multiIndex =
    BOUNDS_CHECK
        (inBounds p multiIndex)
        "index"
        ((summon p)::MultiIndex dims, multiIndex)
        $ unsafeIndex t multiIndex
    where
      p :: Proxy dims
      p = Proxy

-- | Safely access a component of a tensor via a term level multi index.
-- Gives @Nothing@ if the indicated multi index is out of bounds.
-- Example (assuming @N@ and @(:-)@ from
-- @Math.FTensor.SizedList@ are in scope):
--
-- >>> let (x::TensorBoxed '[2, 2] Int) = [[0, 1], [2, 3]]
-- >>> maybeIndex x (1:-0:-N)
-- Just 2
maybeIndex
    :: forall a e (dims::[Nat]).
    ( A.Array a e
    , KnownType dims (MultiIndex dims)
    )
    => Tensor a dims e
    -> MultiIndex dims
    -> Maybe e
maybeIndex t multiIndex
  | inBounds (Proxy::Proxy dims) multiIndex = Just $ unsafeIndex t multiIndex
  | otherwise = Nothing

-- | A tensor with an empty dimension list is essentially a scalar. This
-- function accesses the scalar.
scalar :: A.Array a e => Tensor a '[] e -> e
scalar (Tensor arr) =
    INTERNAL_CHECK
        (A.length arr == 1)
        "scalar"
        (A.length arr)
        $ A.index arr 0

{-# INLINE scalar #-}

-- * Creating

type GenerateConstraint a e (dims::[Nat]) =
    ( A.Array a e
    , KnownType (AllMultiIndicesInBounds dims) [MultiIndex dims]
    , KnownNat (Product dims)
    )

-- | @generate f@: Creates the tensor whose value at component with multi index
-- @mi@ is @f mi@.
generate
    :: forall a e (dims::[Nat]).
    GenerateConstraint a e dims
    => (MultiIndex dims -> e)
    -> Tensor a dims e
generate f = Tensor $ fromListN len (fmap f lists)
  where
    len :: Int
    len = summon (Proxy::Proxy (Product dims))
    lists :: [MultiIndex dims]
    lists = summon (Proxy::Proxy (AllMultiIndicesInBounds dims))

{-# INLINABLE generate #-}

-- | Convert between two tensors with different underlying array types.
convert
    :: (A.Array a e, A.Array b e)
    => Tensor a dims e
    -> Tensor b dims e
convert (Tensor arr) = Tensor $ A.convert arr

{-# INLINE convert #-}

-- | Turn a scalar into a tensor with empty dimension list.
tensor :: A.Array a e => e -> Tensor a '[] e
tensor x = Tensor (A.generate 1 $ const x)

{-# INLINE tensor #-}

-- * Mathematical operations

-- | Multiply each component of the tensor by the given scalar.
scale
    :: (Multiplicative e, A.Array a e)
    => Tensor a dims e
    -> e
    -> Tensor a dims e
scale (Tensor arr) factor =
    Tensor $ A.generate (A.length arr) ((*.factor) . A.index arr)

{-# INLINE scale #-}

-- | Add tensors componentwise.
add
    :: (Additive e, A.Array a e)
    => Tensor a dims e
    -> Tensor a dims e
    -> Tensor a dims e
add (Tensor arr1) (Tensor arr2) =
    INTERNAL_CHECK
        (A.length arr1 == A.length arr2)
        "add"
        (A.length arr1, A.length arr2)
        $ Tensor $ A.generate
            (A.length arr1)
            (\i -> A.index arr1 i +. A.index arr2 i)

{-# INLINE add #-}

-- | Subtract tensors componentwise.
minus
    :: (WithNegatives e, A.Array a e)
    => Tensor a dims e
    -> Tensor a dims e
    -> Tensor a dims e
minus (Tensor arr1) (Tensor arr2) =
    INTERNAL_CHECK
        (A.length arr1 == A.length arr2)
        "minus"
        (A.length arr1, A.length arr2)
        $ Tensor $ A.generate
            (A.length arr1)
            (\i -> A.index arr1 i -. A.index arr2 i)

{-# INLINE minus #-}

type TensorProductConstraint a e (ds1::[Nat]) (ds2::[Nat]) =
    ( A.Array a e
    , Multiplicative e
    , KnownNat (Product ds1)
    , KnownNat (Product ds2)
    )

-- | Find the tensor product of the two tensors, in the basis obtained by
-- the tensor products of the two bases.
tensorProduct
    :: forall a e (ds1::[Nat]) (ds2::[Nat])
    . TensorProductConstraint a e ds1 ds2
    => Tensor a ds1 e
    -> Tensor a ds2 e
    -> Tensor a (ds1 ++ ds2) e
tensorProduct (Tensor arr1) (Tensor arr2) = Tensor $ runST $ do
    -- this is faster than using unfoldN or generate
    newArr <- A.new (len1 * len2)
    write <- A.writer newArr
    let oneRow i =
            let x = A.index arr1 i
            in
            mapM_ (write . (x *.) . (A.index arr2)) [0 .. len2-1]
    mapM_ oneRow [0 .. len1-1]
    A.freeze newArr
  where
    len1 :: Int
    len1 = summon (Proxy::Proxy (Product ds1))
    len2 :: Int
    len2 = summon (Proxy::Proxy (Product ds2))

{-# INLINABLE tensorProduct #-}

loop :: Int -> Int -> Int -> (Int -> ST s ()) -> ST s ()
loop !i !increment !end f
  | i < end = f i >> loop (i+increment) increment end f
  | otherwise = return ()

sumAt :: Additive e => Int -> Int -> Int -> (Int -> ST s e) -> ST s e
sumAt !i !increment !end read = do
    sum <- read i >>= newSTRef
    loop (i+increment) increment end
        (\i -> read i >>= (\k -> modifySTRef' sum (+. k)))
    readSTRef sum

{-# INLINE sumAt #-}

data ContractArguments e s = ContractArguments
    { read :: Int -> ST s e
    , write :: e -> ST s ()
    , ijOffset :: Int
    , ijCount :: Int
    , firstOffset :: Int
    , firstCount :: Int
    , middleOffset :: Int
    , middleCount :: Int
    , lastCount :: Int
    }

contract_ :: Additive e => ContractArguments e s -> ST s ()
contract_ ContractArguments{..} =
    let writeOne !i =
            sumAt i ijOffset (i+ijOffset*ijCount) read >>= write
        innerLoop !i =
            loop i 1 (i+lastCount) writeOne
        middleLoop !i =
            loop i middleOffset (i+middleCount*middleOffset) innerLoop
    in
    loop 0 firstOffset (firstCount*firstOffset) middleLoop

{-# INLINE contract_ #-}

-- | Contract a tensor on the given slots.
--
-- >>> let (x::TensorBoxed '[2,2] Int) = fromList [[1,2],[3,4]]
-- >>> contract x (Proxy::Proxy 0) (Proxy::Proxy 1)
-- 5
contract
    :: forall a e (dims::[Nat]) (i::Nat) (j::Nat)
    . ContractConstraint a e dims i j
    => Tensor a dims e
    -> Proxy i
    -> Proxy j
    -> Tensor a (ContractedDims dims i j) e
contract (Tensor arr) _ _ = Tensor $ runST $ do
    newArr <-
        A.new (summon (Proxy::Proxy (Product (ContractedDims dims i j))))
    writer <- A.writer newArr
    contract_ ContractArguments
        { read = return . A.index arr
        , write = writer
        , ijOffset = summon (Proxy::Proxy (IJOffset dims i j))
        , ijCount = summon (Proxy:: Proxy (dims !! i))
        , firstOffset = summon (Proxy::Proxy (FirstOffset dims i j))
        , firstCount = summon (Proxy::Proxy (FirstCount dims i j))
        , middleOffset = summon (Proxy::Proxy (MiddleOffset dims i j))
        , middleCount = summon (Proxy::Proxy (MiddleCount dims i j))
        , lastCount = summon (Proxy::Proxy (LastCount dims i j))
        }
    A.freeze newArr

{-# INLINABLE contract #-}

type ContractConstraint a e (dims::[Nat]) (i::Nat) (j::Nat) =
    ( Equal i j ~ 'False
    , i+1 <= Length dims
    , j+1 <= Length dims
    , (dims !! i) ~ (dims !! j)
    , KnownNat (dims !! i)
    , KnownNat (IJOffset dims i j)
    , KnownNat (Product (ContractedDims dims i j))
    , KnownNat (FirstOffset dims i j)
    , KnownNat (FirstCount dims i j)
    , KnownNat (MiddleOffset dims i j)
    , KnownNat (MiddleCount dims i j)
    , KnownNat (LastCount dims i j)
    , Additive e
    , A.Array a e
    )

type FirstOffset (dims::[Nat]) (i::Nat) (j::Nat) =
    Product (Drop (Min i j) dims)

type FirstCount (dims::[Nat]) (i::Nat) (j::Nat) =
    Product (Take (Min i j) dims)

type MiddleOffset (dims::[Nat]) (i::Nat) (j::Nat) =
    Product (Drop (Max i j) dims)

type MiddleCount (dims::[Nat]) (i::Nat) (j::Nat) =
    Product (Drop (Min i j+1) (Take (Max i j) dims))

type LastCount (dims::[Nat]) (i::Nat) (j::Nat) =
    Product (Drop (Max i j+1) dims)

type IJOffset (dims::[Nat]) (i::Nat) (j::Nat) =
    Product (Drop (i+1) dims) + Product (Drop (j+1) dims)

type family ContractedDims (xs::[Nat]) (i::Nat) (j::Nat) :: [Nat] where
    ContractedDims xs i j = Delete (Delete xs (Max i j)) (Min i j)

-- | Compute the trace of a square matrix. This is equivalent to contracting
-- the matrix on its two slots.
trace
    :: forall a e (dim::Nat)
    . (Additive e, KnownType dim Int, A.Array a e)
    => Tensor a '[dim, dim] e
    -> e
trace (Tensor arr) = runST $ sumAt 0 dim1 len (return . A.index arr)
  where
    len = A.length arr
    dim1 = 1 + summon (Proxy::Proxy dim)

{-# INLINE trace #-}

-- | Find the dot product of two vectors. This is equivalent to taking the
-- tensor product of the vectors and then contracting on the two slots.
dot
    :: (Additive e, Multiplicative e, A.Array a e)
    => Tensor a '[dim] e
    -> Tensor a '[dim] e
    -> e
dot (Tensor arr1) (Tensor arr2) =
    INTERNAL_CHECK
        (len1 == len2)
        "dot"
        (len1, len2)
        (runST $ sumAt 0 1 len1 (return . g))
  where
    g i = A.index arr1 i *. A.index arr2 i
    len1 = A.length arr1
    len2 = A.length arr2

{-# INLINE dot #-}

type MulConstraint a e (dims1::[Nat]) (dims2::[Nat]) (i::Nat) (j::Nat) =
    ( Additive e
    , Multiplicative e
    , A.Array a e
    , (dims1 !! i) ~ (dims2 !! j)
    , KnownNat (Product (MulNewDims dims1 i dims2 j))
    , KnownNat (IOffset dims1 i)
    , KnownNat (IOffset dims2 j)
    , KnownNat (dims1 !! i)
    , KnownNat (MulFirstOffset dims1 i)
    , KnownNat (MulFirstCount dims1 i)
    , KnownNat (MulFirstOffset dims2 j)
    , KnownNat (MulFirstCount dims2 j)
    )

data MulArguments e s = MulArguments
    { read1 :: Int -> ST s e
    , read2 :: Int -> ST s e
    , mWrite :: e -> ST s ()
    , iOffset1 :: Int
    , iOffset2 :: Int
    , iCount :: Int
    , firstOffset1 :: Int
    , firstCount1 :: Int
    , lastCount1 :: Int
    , firstOffset2 :: Int
    , firstCount2 :: Int
    , lastCount2 :: Int
    }

loop2 :: Int -> Int -> Int -> Int -> Int -> (Int -> Int -> ST s ()) -> ST s ()
loop2 !i !iIncrement !iEnd !j !jIncrement f
  | i < iEnd =
      f i j >> loop2 (i+iIncrement) iIncrement iEnd (j+jIncrement) jIncrement f
  | otherwise = return ()

sumAt2
    :: Additive e
    => Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> (Int -> Int -> ST s e)
    -> ST s e
sumAt2 !i !iIncrement !iEnd !j !jIncrement read = do
    sum <- read i j >>= newSTRef
    loop2 (i+iIncrement) iIncrement iEnd (j+jIncrement) jIncrement
        (\i j -> read i j >>= (\k -> modifySTRef' sum (+. k)))
    readSTRef sum

{-# INLINE sumAt2 #-}

mul_ :: (Additive e, Multiplicative e) => MulArguments e s -> ST s ()
mul_ MulArguments{..} =
    let writeOne !i !j =
            sumAt2 i iOffset1 (i+iOffset1*iCount) j iOffset2
                (\i j -> do
                    iV <- read1 i
                    jV <- read2 j
                    return (iV *. jV)
                    ) >>= mWrite
        innerLoop !i !j =
            loop j 1 (j+lastCount2) (writeOne i)
        innerMiddleLoop !i =
            loop 0 firstOffset2 (firstOffset2*firstCount2) (innerLoop i)
        outerMiddleLoop !i =
            loop i 1 (i+lastCount1) innerMiddleLoop
    in
    loop 0 firstOffset1 (firstOffset1*firstCount1) outerMiddleLoop

{-# INLINE mul_ #-}

-- | Applying @mul@ to two tensors is equivalent to taking their
-- @tensorProduct@ and then contracting on the two adjusted slots.
mul
    :: forall a e (dims1::[Nat]) (dims2::[Nat]) (i::Nat) (j::Nat)
    . MulConstraint a e dims1 dims2 i j
    => Tensor a dims1 e
    -> Proxy i
    -> Tensor a dims2 e
    -> Proxy j
    -> Tensor a (Delete dims1 i ++ Delete dims2 j) e
mul (Tensor arr1) _ (Tensor arr2) _ = Tensor $ runST $ do
    newArr <-
        A.new (summon (Proxy::Proxy (Product (MulNewDims dims1 i dims2 j))))
    writer <- A.writer newArr
    mul_ MulArguments
        { read1 = return . A.index arr1
        , read2 = return . A.index arr2
        , mWrite = writer
        , iOffset1 = summon (Proxy::Proxy (IOffset dims1 i))
        , iOffset2 = summon (Proxy::Proxy (IOffset dims2 j))
        , iCount = summon (Proxy::Proxy (dims1 !! i))
        , firstOffset1 = summon (Proxy::Proxy (MulFirstOffset dims1 i))
        , firstCount1 = summon (Proxy::Proxy (MulFirstCount dims1 i))
        , lastCount1 = summon (Proxy::Proxy (IOffset dims1 i))
        , firstOffset2 = summon (Proxy::Proxy (MulFirstOffset dims2 j))
        , firstCount2 = summon (Proxy::Proxy (MulFirstCount dims2 j))
        , lastCount2 = summon (Proxy::Proxy (IOffset dims2 j))
        }
    A.freeze newArr

{-# INLINABLE mul #-}

type MulNewDims (dims1::[Nat]) (i::Nat) (dims2::[Nat]) (j::Nat) =
    Delete dims1 i ++ Delete dims2 j

type IOffset (dims::[Nat]) (n::Nat) = Product (Drop (n+1) dims)

type MulFirstOffset (dims::[Nat]) (n::Nat) = Product (Drop n dims)

type MulFirstCount (dims::[Nat]) (n::Nat) = Product (Take n dims)

type ChangeBasisConstraint a m e dims dim slots =
    ( Additive e
    , Multiplicative e
    , A.Array a e
    , ChangeBasisClass dims dim slots
    )

changeBasis
    :: ChangeBasisConstraint a m e dims dim slots
    => Tensor a dims e
    -> Tensor a [dim, dim] e
    -> Proxy slots
    -> Tensor a dims e
changeBasis = changeBasis_

{-# INLINABLE changeBasis #-}

data ChangeBasisArguments e s = ChangeBasisArguments
    { readT :: Int -> ST s e
    , readM :: Int -> ST s e
    , writeT :: e -> ST s ()
    , aOffset :: Int
    , aCount :: Int
    , bOffset :: Int
    , bCount :: Int
    , cCount :: Int
    }

changeBasis1_
    :: (Additive e, Multiplicative e)
    => ChangeBasisArguments e s 
    -> ST s ()
changeBasis1_ ChangeBasisArguments{..} = do
    loop 0 aOffset (aOffset*aCount) middleLoop
  where
    middleLoop !outerOffset =
        loop 0 bCount (bCount*bCount) (innerLoop outerOffset)
    innerLoop !outerOffset !idxM =
        loop outerOffset 1 (outerOffset + cCount) (writeOne idxM)
    writeOne !idxM !idx = do
        sumAt2 idxM 1 (idxM+bCount) idx bOffset
            (\j i -> do
                iV <- readT i
                jV <- readM j
                return (iV *. jV)
                ) >>= writeT

{-# INLINE changeBasis1_ #-}

class ChangeBasisClass (dims::[Nat]) (dim::Nat) (slots::[Nat]) where
    changeBasis_
        :: forall a e
        . (Additive e, Multiplicative e, A.Array a e)
        => Tensor a dims e
        -> Tensor a [dim, dim] e
        -> Proxy slots
        -> Tensor a dims e

instance ChangeBasisClass dims dim '[] where
    {-# INLINE changeBasis_ #-}
    changeBasis_ t _ _ = t

instance
    ( ChangeBasisClass dims dim slots
    , (dims !! slot) ~ dim
    , NotIn slots slot
    , KnownNat (AOffset dims slot)
    , KnownNat (ACount dims slot)
    , KnownNat dim
    , KnownNat (CCount dims slot)
    )
    => ChangeBasisClass dims dim (slot ': slots) where

    {-# INLINE changeBasis_ #-}
    changeBasis_ (Tensor arrT) m@(Tensor arrM) _ =
        changeBasis_ t m (Proxy::Proxy slots)
      where
        t = Tensor $ runST $ do
            newArr <- A.new (A.length arrT)
            writer <- A.writer newArr
            changeBasis1_ ChangeBasisArguments
                { readT = return . A.index arrT
                , readM = return . A.index arrM
                , writeT = writer
                , aOffset = summon (Proxy::Proxy (AOffset dims slot))
                , aCount = summon (Proxy::Proxy (ACount dims slot))
                , bOffset = summon (Proxy::Proxy (BOffset dims slot))
                , bCount = summon (Proxy::Proxy dim)
                , cCount = summon (Proxy::Proxy (CCount dims slot))
                }
            A.freeze newArr

type AOffset (dims::[Nat]) slot = Product (Drop slot dims)

type ACount (dims::[Nat]) slot = Product (Take slot dims)

type BOffset (dims::[Nat]) slot = Product (Drop (slot+1) dims)

type CCount (dims::[Nat]) slot = BOffset dims slot

type family NotIn (list::[Nat]) (item::Nat) :: Constraint where
    NotIn '[] item = ()
    NotIn (x ': xs) x = 'True ~ 'False
    NotIn (x ': xs) y = NotIn xs y

changeBasisAll
    :: forall a m e dims dim
    . ChangeBasisConstraint a m e dims dim (EnumFromTo 0 (Length dims - 1))
    => Tensor a dims e
    -> Tensor a [dim, dim] e
    -> Tensor a dims e
changeBasisAll t m =
    changeBasis t m (Proxy::Proxy (EnumFromTo 0 (Length dims - 1)))

{-# INLINE changeBasisAll #-}

instance (Additive e, A.Array a e) => Additive (Tensor a dims e) where
    {-# INLINE (+.) #-}
    (+.) = add

instance
    ( WithZero e
    , A.Array a e
    , KnownNat (Product dims)
    )
    => WithZero (Tensor a dims e) where

    {-# INLINE zero #-}
    zero = Tensor $ A.replicate (summon (Proxy::Proxy (Product dims))) zero

instance
    ( WithNegatives e
    , WithZero (Tensor a dims e)
    , A.Array a e
    )
    => WithNegatives (Tensor a dims e) where

    {-# INLINE neg #-}
    neg = \(Tensor arr) -> Tensor $ A.map neg arr

    {-# INLINE (-.) #-}
    (-.) = minus

instance (Multiplicative e, A.Array a e)
    => WithScalars (Tensor a dims e) where

    type Scalar (Tensor a dims e) = e

    {-# INLINE (*:) #-}
    (*:) = flip scale

instance (Multiplicative e, A.Array a e)
    => Multiplicative (Tensor a '[] e) where

    {-# INLINE (*.) #-}
    lhs *. rhs = tensor (scalar lhs *. scalar rhs)

instance (WithOne e, A.Array a e)
    => WithOne (Tensor a '[] e) where

    {-# INLINE one #-}
    one = tensor one

instance (WithReciprocals e, A.Array a e)
    => WithReciprocals (Tensor a '[] e) where

    {-# INLINE invertible #-}
    invertible = invertible . scalar

    {-# INLINE inv #-}
    inv = tensor . inv . scalar

    {-# INLINE (/.) #-}
    lhs /. rhs = tensor (scalar lhs /. scalar rhs)
