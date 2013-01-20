{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances,
 FunctionalDependencies, KindSignatures,
  MultiParamTypeClasses, NoImplicitPrelude,
  TypeOperators, UndecidableInstances  #-}
{-# OPTIONS -Wall #-}
-- | A tensor algebra library. Main ingredients are :
--
-- 'Vec' and ':~' are data constructors for rank-1 tensor.
-- This is essentially a touple of objects of the same type.
--
-- 'Vector' is a class for rank-1 tensor.
--
-- 'Axis' is an object for accessing the tensor components.

module Data.Tensor.TypeLevel
    (
     (:~)(..), Vec(..), Axis(..), (!),
     Vector(..), VectorRing(..),
     contract,
     Vec0, Vec1, Vec2, Vec3, Vec4,
     Vec5, Vec6, Vec7, Vec8, Vec9, Vec10,
     vec0, vec1, vec2, vec3, vec4,
     vec5, vec6, vec7, vec8, vec9, vec10
    ) where

import qualified Algebra.Additive as Additive
import qualified Algebra.Ring as Ring
import           System.IO.Unsafe
import           Text.Read
import qualified Text.ParserCombinators.ReadP as P

import           Control.Applicative
import           Control.Monad hiding
    (mapM_, sequence_, forM_, msum, mapM, sequence, forM)
import           Data.Foldable
import           Data.List (intercalate)
import           Data.Traversable
import           NumericPrelude hiding
    (Monad, Functor, (*>),
     (>>=), (>>), return, fail, fmap, mapM, mapM_, sequence, sequence_,
     (=<<), foldl, foldl1, foldr, foldr1, and, or, any, all, sum, product,
     concat, concatMap, maximum, minimum, elem, notElem)
import qualified Prelude as P98
import qualified Test.QuickCheck.Arbitrary as QC

infixl 9 !
-- | a component operator.
(!) :: Vector v => v a -> Axis v -> a
v ! i  = component i v

-- | data constructor for 0-dimensional tensor.
data Vec a
  = Vec
  deriving (Eq, Ord)

-- | data constructor for constructing n+1-dimensional tensor
-- from n-dimensional tensor.
data (n :: * -> * ) :~ a
  = (n a) :~ a
  deriving (Eq)
infixl 3 :~

instance Show (Vec a) where
  show = const "()"

instance (Show a, Traversable ((:~) n)) => Show (n :~ a) where
  show = ("("++) . (++")") .
         intercalate "," . map show . toList

instance Read (Vec a) where
  readsPrec _ = P.readP_to_S $ do
    _ <- P.string "()"
    return Vec

instance (Read a, Vector ((:~) n)) => Read (n :~ a) where
  readsPrec _ = P.readP_to_S $ do
    _ <- P.char '('
    ret <- sequence $ compose (\(Axis i) -> do
                when (i>0) $ (P.char ',' >> return ())
                P.readS_to_P (readsPrec 0)
            )
    _ <- P.char ')'
    return ret

-- | the last component contributes the most to the ordering
instance (Ord (n a), Ord a) => Ord (n :~ a) where
  compare (xs :~ x) (ys :~ y) = compare (x, xs) (y, ys)

instance Foldable Vec where
  foldMap = foldMapDefault
instance Functor Vec where
  fmap = fmapDefault
instance Traversable Vec where
  traverse _ Vec = pure Vec
instance Applicative Vec where
  pure _  = Vec
  _ <*> _ = Vec

instance (Traversable n) => Foldable ((:~) n) where
  foldMap = foldMapDefault
instance (Traversable n) => Functor ((:~) n) where
  fmap = fmapDefault
instance (Traversable n) => Traversable ((:~) n) where
  traverse f (x :~ y) = (:~) <$> traverse f x <*> f y
instance (Applicative n, Traversable n) => Applicative ((:~) n) where
  pure x = pure x :~ x
  (vf :~ f) <*> (vx :~ x) = (vf <*> vx) :~ (f x)



-- | An coordinate 'Axis' , labeled by an integer.
-- Axis also carries v, the container type for its corresponding
-- vector. Therefore, An axis of one type can access only vectors
-- of a fixed dimension, but of arbitrary type.
newtype Axis (v :: * -> *) = Axis {axisIndex::Int} deriving (Eq,Ord,Show,Read)

-- | An object that allows component-wise access.
class (Traversable v) => Vector v where
  -- | Get a component within f, a context which allows failure.
  componentF :: (Alternative f) =>
                Axis v -- ^the axis of the component you want
                -> v a -- ^the target vector
                -> f a -- ^the component, obtained within a failure

  -- | Get a component. This computation may result in a runtime error,
  -- though, as long as the 'Axis' is generated from library functions
  -- such as 'compose', there will be no error.
  component :: Axis v -> v a -> a
  component axis vec = case componentF axis vec of
    Just x  -> x
    Nothing -> error $ "axis out of bound: " ++ show axis
  -- | The dimension of the vector.
  dimension :: v a -> Int
  -- | Create a 'Vector' from a function that maps
  -- axis to components.
  compose :: (Axis v -> a) -> v a

instance Vector Vec where
  componentF axis Vec
    = empty
  dimension _ = 0
  compose _ = Vec

instance (Vector v) => Vector ((:~) v) where
  componentF (Axis i) vx@(v :~ x)
    | i==dimension vx - 1 = pure x
    | True                = componentF (Axis i) v
  dimension (v :~ _) = 1 + dimension v
  compose f = let
    xs = compose (\(Axis i)->f (Axis i)) in xs :~ f (Axis (dimension xs))

-- | Vector whose components are additive is also additive.
instance (Additive.C a) => Additive.C (Vec a) where
  zero = compose $ const Additive.zero
  x+y  = compose (\i ->  x!i +  y!i)
  x-y  = compose (\i ->  x!i -  y!i)
  negate x = compose (\i -> negate $ x!i)

instance (Vector v, Additive.C a) => Additive.C ((:~) v a) where
  zero = compose $ const Additive.zero
  x+y  = compose (\i -> x!i + y!i)
  x-y  = compose (\i -> x!i - y!i)
  negate x = compose (\i -> negate $ x!i)

-- | Tensor contraction. Create a 'Vector' from a function that maps
-- axis to component, then sums over the axis and returns @a@.
contract :: (Vector v, Additive.C a) => (Axis v -> a) -> a
contract f = foldl (+) Additive.zero (compose f)



-- | 'VectorRing' is a 'Vector' whose components belongs to 'Ring.C',
-- thus providing unit vectors.
class  (Vector v, Ring.C a) => VectorRing v a where
  -- | A vector where 'Axis'th component is unity but others are zero.
  unitVectorF :: (Alternative f) => Axis v -> f (v a)
  -- | pure but unsafe version means of obtaining a 'unitVector'
  unitVector :: Axis v -> v a
  unitVector axis = case unitVectorF axis of
    Just x  -> x
    Nothing -> error $ "axis out of bound: " ++ show axis

instance (Ring.C a) => VectorRing Vec a where
  unitVectorF axis = empty

instance (Ring.C a, VectorRing v a, Additive.C (v a))
    => VectorRing ((:~) v) a where
  unitVectorF axis@(Axis i) = ret
    where
      z = Additive.zero
      d = dimension z
      ret
        | i < 0 || i >= d   = empty
        | i == d-1          = pure $ Additive.zero :~ Ring.one
        | 0 <= i && i < d-1 = fmap (:~ Additive.zero) $ unitVectorF (Axis i)
        | True              = pure z
        -- this last guard never matches, but needed to infer the type of z.

instance (Vector v, P98.Num a) => P98.Num ((:~) v a) where
  x+y  = compose (\i -> x!i P98.+ y!i)
  x-y  = compose (\i -> x!i P98.- y!i)
  negate x = compose (\i -> P98.negate $ x!i)
  (*) = error "P98 legacy instance"
  fromInteger x = compose $ const (P98.fromInteger x)
  abs = error "P98 legacy instance"
  signum = error "P98 legacy instance"

-- | Type synonyms
type Vec0 = Vec
type Vec1 = (:~) Vec0
type Vec2 = (:~) Vec1
type Vec3 = (:~) Vec2
type Vec4 = (:~) Vec3
type Vec5 = (:~) Vec4
type Vec6 = (:~) Vec5
type Vec7 = (:~) Vec6
type Vec8 = (:~) Vec7
type Vec9 = (:~) Vec8
type Vec10 = (:~) Vec9

-- | Utility functions
vec0 :: Vec0 a
vec0  = Vec
vec1 :: a -> Vec1 a
vec1 x0 = Vec :~ x0
vec2 :: a -> a -> Vec2 a
vec2 x0 x1 = Vec :~ x0 :~ x1
vec3 :: a -> a -> a -> Vec3 a
vec3 x0 x1 x2 = Vec :~ x0 :~ x1 :~ x2
vec4 :: a -> a -> a -> a -> Vec4 a
vec4 x0 x1 x2 x3 = Vec :~ x0 :~ x1 :~ x2 :~ x3
vec5 :: a -> a -> a -> a -> a -> Vec5 a
vec5 x0 x1 x2 x3 x4 = Vec :~ x0 :~ x1 :~ x2 :~ x3 :~ x4
vec6 :: a -> a -> a -> a -> a -> a -> Vec6 a
vec6 x0 x1 x2 x3 x4 x5 = Vec :~ x0 :~ x1 :~ x2 :~ x3 :~ x4 :~ x5
vec7 :: a -> a -> a -> a -> a -> a -> a -> Vec7 a
vec7 x0 x1 x2 x3 x4 x5 x6 = Vec :~ x0 :~ x1 :~ x2 :~ x3 :~ x4 :~ x5 :~ x6
vec8 :: a -> a -> a -> a -> a -> a -> a -> a -> Vec8 a
vec8 x0 x1 x2 x3 x4 x5 x6 x7 = Vec :~ x0 :~ x1 :~ x2 :~ x3 :~ x4 :~ x5 :~ x6 :~ x7
vec9 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> Vec9 a
vec9 x0 x1 x2 x3 x4 x5 x6 x7 x8 = Vec :~ x0 :~ x1 :~ x2 :~ x3 :~ x4 :~ x5 :~ x6 :~ x7 :~ x8
vec10 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Vec10 a
vec10 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 = Vec :~ x0 :~ x1 :~ x2 :~ x3 :~ x4 :~ x5 :~ x6 :~ x7 :~ x8 :~ x9
