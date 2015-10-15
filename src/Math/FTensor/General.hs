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
    consume _ f e = f e
    build _ f = f

instance (BuilderConsumer contained e lengths, KnownNat length)
    => BuilderConsumer [contained] e (length ': lengths) where

    consume _ f list =
        UNSAFE_CHECK
            (length' == Prelude.length list)
            "fromList"
            (length', Prelude.length list)
            $ traverse_ (consume (Proxy::Proxy lengths) f) list
      where
        length' :: Int
        length' = summon (Proxy::Proxy length)

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
            idx <- newSTRef (0::Int)
            let at x = do
                    i <- readSTRef idx
                    A.write newArr i x
                    writeSTRef idx (i+1)
            consume (Proxy::Proxy dims) at list
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

pIndex
    :: forall a e (dims::[Nat]) (multiIndex::[Nat]).
    PIndexConstraint a e dims multiIndex
    => Tensor a dims e
    -> Proxy multiIndex
    -> e
pIndex (Tensor arr) p = A.index arr (multiIndexToI' (Proxy::Proxy dims) p)

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

scalar :: A.Array a e => Tensor a '[] e -> e
scalar (Tensor arr) =
    INTERNAL_CHECK
        (A.length arr == 1)
        "scalar"
        (A.length arr)
        $ A.index arr 0

-- * Creating

type GenerateConstraint a e (dims::[Nat]) =
    ( A.Array a e
    , KnownType (AllMultiIndicesInBounds dims) [MultiIndex dims]
    , KnownNat (Product dims)
    )

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

{-# INLINE[1] convert #-}
convert
    :: (A.Array a e, A.Array b e)
    => Tensor a dims e
    -> Tensor b dims e
convert (Tensor arr) = Tensor $ A.convert arr

{-# RULES "convert/id" convert = id #-}

tensor :: A.Array a e => e -> Tensor a '[] e
tensor x = Tensor (A.generate 1 $ const x)

-- * Mathematical operations

scale
    :: (Multiplicative e, A.Array a e)
    => Tensor a dims e
    -> e
    -> Tensor a dims e
scale (Tensor arr) factor =
    Tensor $ A.generate (A.length arr) ((*.factor) . A.index arr)

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

type TensorProductConstraint a e (ds1::[Nat]) (ds2::[Nat]) =
    ( A.Array a e
    , Multiplicative e
    , KnownNat (Product ds1)
    , KnownNat (Product ds2)
    )

tensorProduct
    :: forall a e (ds1::[Nat]) (ds2::[Nat])
    . TensorProductConstraint a e ds1 ds2
    => Tensor a ds1 e
    -> Tensor a ds2 e
    -> Tensor a (ds1 ++ ds2) e
tensorProduct (Tensor arr1) (Tensor arr2) = Tensor $ runST $ do
    -- this is faster than using unfoldN or generate
    newArr <- A.new (len1 * len2)
    let oneRow = \i ->
         let x = A.index arr1 i
             i' = i*len2
         in
         mapM_
            (\j -> A.write newArr (j+i') (x *. A.index arr2 j))
            [0 .. len2-1]
    mapM_ oneRow [0..len1-1]
    A.freeze newArr
  where
    len1 :: Int
    len1 = summon (Proxy::Proxy (Product ds1))
    len2 :: Int
    len2 = summon (Proxy::Proxy (Product ds2))

data ContractArguments e s = ContractArguments
    { read :: Int -> ST s e
    , write :: Int -> e -> ST s ()
    , ijOffset :: Int
    , ijCount :: Int
    , firstOffset :: Int
    , firstCount :: Int
    , middleOffset :: Int
    , middleCount :: Int
    , lastCount :: Int
    }

contract_ :: Additive e => ContractArguments e s -> ST s ()
contract_ ContractArguments{..} = newSTRef (0::Int) >>= \writeIndex ->
    let writeOne i = do
            writeIndex' <- readSTRef writeIndex
            sumStartingAt i >>= write writeIndex'
            writeSTRef writeIndex (writeIndex' + 1)
        innerLoop !i =
            mapM_ writeOne [i, i+1 .. i+lastCount-1]
        middleLoop !i =
            mapM_ innerLoop [i, i+middleOffset .. i+middleCount*middleOffset-1]

        sumStartingAt i = read i >>= f (i+ijOffset*ijCount) (i+ijOffset) 
        f !max !idx !sum
          | idx < max = do
                val <- read idx
                f max (idx+ijOffset) (sum +. val)
          | otherwise = return sum
    in
    mapM_ middleLoop [0, firstOffset .. firstCount*firstOffset-1]

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
    contract_ ContractArguments
        { read = return . A.index arr
        , write = A.write newArr
        , ijOffset = summon (Proxy::Proxy (IJOffset dims i j))
        , ijCount = summon (Proxy:: Proxy (dims !! i))
        , firstOffset = summon (Proxy::Proxy (FirstOffset dims i j))
        , firstCount = summon (Proxy::Proxy (FirstCount dims i j))
        , middleOffset = summon (Proxy::Proxy (MiddleOffset dims i j))
        , middleCount = summon (Proxy::Proxy (MiddleCount dims i j))
        , lastCount = summon (Proxy::Proxy (LastCount dims i j))
        }
    A.freeze newArr

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

type family Offsets (dims::[Nat]) :: [Nat] where
    Offsets '[] = '[]
    Offsets (dim ': dims) = Offsets_ dims

type family Offsets_ (dims::[Nat]) :: [Nat] where
    Offsets_ '[] = '[1]
    Offsets_ (dim ': dims) = (dim * Head (Offsets_ dims)) ': Offsets_ dims

trace
    :: forall a e (dim::Nat)
    . (Additive e, KnownType dim Int, A.Array a e)
    => Tensor a '[dim, dim] e
    -> e
trace (Tensor arr) = f dim1 (A.index arr 0)
  where
    f !i !sum
      | i < len = f (i + dim1) (sum +. A.index arr i)
      | otherwise = sum
    len = A.length arr
    dim1 = 1 + summon (Proxy::Proxy dim)

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
        f 1 (g 0)
  where
    f !i !sum
      | i < len1 = f (i+1) (sum +. g i)
      | otherwise = sum
    g i = A.index arr1 i *. A.index arr2 i
    len1 = A.length arr1
    len2 = A.length arr2

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
    , mWrite :: Int -> e -> ST s ()
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

mul_ :: (Additive e, Multiplicative e) => MulArguments e s -> ST s ()
mul_ MulArguments{..} = newSTRef (0::Int) >>= \writeIndex ->
    let writeOne !i !j = do
            writeIndex' <- readSTRef writeIndex
            sumStartingAt i j >>= mWrite writeIndex'
            writeSTRef writeIndex (writeIndex' + 1)
        innerLoop !i !j =
            mapM_ (writeOne i) [j, j+1 .. j + lastCount2 - 1]
        innerMiddleLoop !i =
            mapM_ (innerLoop i)
                [0, firstOffset2 .. firstOffset2*firstCount2 - 1]
        outerMiddleLoop !i =
            mapM_ innerMiddleLoop [i, i+1 .. i + lastCount1 - 1]
        sumStartingAt !i !j = do
            i' <- read1 i
            j' <- read2 j
            f (i + iOffset1*iCount) (i + iOffset1) (j + iOffset2) (i'*.j')
        f !iMax !i !j !sum
          | i < iMax = do
                i' <- read1 i
                j' <- read2 j
                f iMax (i + iOffset1) (j + iOffset2) (sum +. i' *. j')
          | otherwise = return sum
    in
    mapM_ outerMiddleLoop [0, firstOffset1 .. firstOffset1*firstCount1 - 1]

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
    mul_ MulArguments
        { read1 = return . A.index arr1
        , read2 = return . A.index arr2
        , mWrite = A.write newArr
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

type MulNewDims (dims1::[Nat]) (i::Nat) (dims2::[Nat]) (j::Nat) =
    Delete dims1 i ++ Delete dims2 j

type IOffset (dims::[Nat]) (n::Nat) = Product (Drop (n+1) dims)

type MulFirstOffset (dims::[Nat]) (n::Nat) = Product (Drop n dims)

type MulFirstCount (dims::[Nat]) (n::Nat) = Product (Take n dims)

type family AllEq (n::Nat) (ns::[Nat]) :: Bool where
    AllEq n '[] = 'True
    AllEq n (m ': ms) = And (Equal n m) (AllEq n ms)

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

data ChangeBasisArguments e s = ChangeBasisArguments
    { readT :: Int -> ST s e
    , readM :: Int -> ST s e
    , writeT :: Int -> e -> ST s ()
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
    writeIdx <- newSTRef 0
    mapM_ (writeOne writeIdx) $ do
        outerOffset <- [0, aOffset .. aOffset*aCount - 1]
        idxM <- [0, bCount .. bCount*bCount - 1]
        idx <- [outerOffset, outerOffset + 1 ..
            outerOffset + cCount - 1]
        return (idx, idxM)
  where
    writeOne !writeIdx (!idx, !idxM) = do
        i <- readSTRef writeIdx
        sumAt idx idxM >>= writeT i
        writeSTRef writeIdx (i+1)
    sumAt idx idxM = do
        val1 <- readT idx
        val2 <- readM idxM
        f (idx+bOffset) (idxM+1) (idxM+bCount) (val1 *. val2)
    f !idx !idxM !idxMMax !sum
      | idxM < idxMMax = do
        i' <- readT idx
        j' <- readM idxM
        f (idx + bOffset) (idxM + 1) idxMMax (sum +. i' *. j')
      | otherwise = return sum


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
    changeBasis_ (Tensor arrT) m@(Tensor arrM) _ =
        changeBasis_ t m (Proxy::Proxy slots)
      where
        t = Tensor $ runST $ do
            newArr <- A.new (A.length arrT)
            changeBasis1_ ChangeBasisArguments
                { readT = return . A.index arrT
                , readM = return . A.index arrM
                , writeT = A.write newArr
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
    :: forall a e (dim::Nat) (dims::[Nat])
    . (Additive e, Multiplicative e, AllEq dim dims ~ 'True,
      dim ~ (dims !! 0),
      KnownNat dim, GenerateConstraint a e dims,
      KnownType dims (MultiIndex dims))
    => Tensor a dims e
    -> Tensor a [dim, dim] e
    -> Tensor a dims e
changeBasisAll t m = generate f
  where
    f multiIndex =
        foldr1 (+.) $ map (\mi -> indexMat mi multiIndex *. index t mi) lists
    indexMat :: SizedList s Int -> SizedList s Int -> e
    indexMat (i:-N) (k:-N) = index m (k:-i:-N)
    indexMat (i:-is) (k:-ks) = index m (k:-i:-N) *. indexMat is ks
    indexMat _ _ = error "changeBasisAll: can't happen"
    lists :: [MultiIndex dims]
    lists = summon (Proxy::Proxy (AllMultiIndicesInBounds dims))

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
