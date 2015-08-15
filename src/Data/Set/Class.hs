{-# LANGUAGE
    NoImplicitPrelude
  , MultiParamTypeClasses
  , UndecidableInstances
  , FlexibleInstances
  , FlexibleContexts
  , GeneralizedNewtypeDeriving
  , StandaloneDeriving
  #-}

-- | Convenience operators overloaded for arbitrary use.
-- There are no laws associated with these classes, just duck-typed so
-- we don't have to use the qualified versions of each function.

module Data.Set.Class where

import Prelude (Eq (..), Ord, Int, Bool (..), (&&), (||), ($), (.), not, const)
import Data.Foldable as Fold
import Data.Monoid as Monoid
import Data.Commutative as Comm

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import Data.Hashable (Hashable)
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Functor.Contravariant as Pred
import qualified Data.Set.Ordered.Many as OM
import Data.Discrimination as Disc
import qualified Data.Set.Unordered.Many as UM
import qualified Data.Set.Unordered.Unique as UU
import qualified Data.Set.Ordered.Unique.Finite as OUF
import qualified Data.Set.Ordered.Unique.With as SetWith


newtype Union a        = Union {unUnion :: a}
newtype Intersection a = Intersection {unIntersection :: a}
newtype XUnion a       = XUnion {unXUnion :: a}

class HasUnion s where
  union :: s -> s -> s

unions :: ( Fold.Foldable f
          , HasUnion s
          , HasEmpty s
          ) => f s -> s
unions = foldr Data.Set.Class.union empty

instance HasUnion s => Commutative (Union s) where
  commute = union

instance (HasUnion s, HasEmpty s) => Monoid.Monoid (Union s) where
  mappend = union
  mempty = empty

class HasDifference s where
  difference :: s -> s -> s

class HasIntersection s where
  intersection :: s -> s -> s

intersections :: ( Fold.Foldable f
                 , HasIntersection s
                 , HasTotal s
                 ) => f s -> s
intersections = foldr Data.Set.Class.intersection total

instance HasIntersection s => Commutative (Intersection s) where
  commute = intersection

instance (HasIntersection s, HasTotal s) => Monoid.Monoid (Intersection s) where
  mappend = intersection
  mempty = total

class HasXUnion s where
  xunion :: s -> s -> s

instance (HasUnion s, HasIntersection s, HasDifference s) => HasXUnion s where
  xunion x y = union x y `difference` intersection x y

instance (HasXUnion s, HasUnion s, HasIntersection s, HasDifference s) => Commutative (XUnion s) where
  commute = xunion

instance (HasXUnion s, HasEmpty s, HasUnion s, HasIntersection s, HasDifference s) => Monoid.Monoid (XUnion s) where
  mappend = xunion
  mempty = empty


class HasComplement s where
  complement :: s -> s

class HasSingleton a s where
  singleton :: a -> s

class HasSingletonWith k a s where
  singletonWith :: k -> a -> s

class HasDelete a s where
  delete :: a -> s -> s

class HasInsert a s where
  insert :: a -> s -> s

class HasInsertWith k a s where
  insertWith :: k -> a -> s -> s

class HasEmpty s where
  empty :: s

instance (Commutative (Union s), HasEmpty s) => CommutativeId (Union s) where
  cempty = empty

class HasEmptyWith k s where
  emptyWith :: k -> s

class HasTotal s where
  total :: s

instance (Commutative (Intersection s), HasTotal s) => CommutativeId (Intersection s) where
  cempty = total

class HasTotalWith k s where
  totalWith :: k -> s

class HasSize s where
  size :: s -> Int

class CanBeSubset s where
  isSubsetOf :: s -> s -> Bool

class CanBeProperSubset s where
  isProperSubsetOf :: s -> s -> Bool


-- Instances

-- Inherit
deriving instance HasUnion a             => HasUnion             (Union a)
deriving instance HasDifference a        => HasDifference        (Union a)
deriving instance HasIntersection a      => HasIntersection      (Union a)
deriving instance HasComplement a        => HasComplement        (Union a)
deriving instance HasSingleton x a       => HasSingleton x       (Union a)
deriving instance HasSingletonWith k x a => HasSingletonWith k x (Union a)
deriving instance HasInsert x a          => HasInsert x          (Union a)
deriving instance HasInsertWith k x a    => HasInsertWith k x    (Union a)
deriving instance HasDelete x a          => HasDelete x          (Union a)
deriving instance HasEmpty a             => HasEmpty             (Union a)
deriving instance HasEmptyWith k a       => HasEmptyWith k       (Union a)
deriving instance HasTotal a             => HasTotal             (Union a)
deriving instance HasTotalWith k a       => HasTotalWith  k      (Union a)
deriving instance HasSize a              => HasSize              (Union a)
deriving instance CanBeSubset a          => CanBeSubset          (Union a)
deriving instance CanBeProperSubset a    => CanBeProperSubset    (Union a)
deriving instance HasUnion a             => HasUnion             (Intersection a)
deriving instance HasDifference a        => HasDifference        (Intersection a)
deriving instance HasIntersection a      => HasIntersection      (Intersection a)
deriving instance HasComplement a        => HasComplement        (Intersection a)
deriving instance HasSingleton x a       => HasSingleton x       (Intersection a)
deriving instance HasSingletonWith k x a => HasSingletonWith k x (Intersection a)
deriving instance HasInsert x a          => HasInsert x          (Intersection a)
deriving instance HasInsertWith k x a    => HasInsertWith k x    (Intersection a)
deriving instance HasDelete x a          => HasDelete x          (Intersection a)
deriving instance HasEmpty a             => HasEmpty             (Intersection a)
deriving instance HasEmptyWith k a       => HasEmptyWith k       (Intersection a)
deriving instance HasTotal a             => HasTotal             (Intersection a)
deriving instance HasTotalWith k a       => HasTotalWith  k      (Intersection a)
deriving instance HasSize a              => HasSize              (Intersection a)
deriving instance CanBeSubset a          => CanBeSubset          (Intersection a)
deriving instance CanBeProperSubset a    => CanBeProperSubset    (Intersection a)
deriving instance HasUnion a             => HasUnion             (XUnion a)
deriving instance HasDifference a        => HasDifference        (XUnion a)
deriving instance HasIntersection a      => HasIntersection      (XUnion a)
deriving instance HasComplement a        => HasComplement        (XUnion a)
deriving instance HasSingleton x a       => HasSingleton x       (XUnion a)
deriving instance HasSingletonWith k x a => HasSingletonWith k x (XUnion a)
deriving instance HasInsert x a          => HasInsert x          (XUnion a)
deriving instance HasInsertWith k x a    => HasInsertWith k x    (XUnion a)
deriving instance HasDelete x a          => HasDelete x          (XUnion a)
deriving instance HasEmpty a             => HasEmpty             (XUnion a)
deriving instance HasEmptyWith k a       => HasEmptyWith k       (XUnion a)
deriving instance HasTotal a             => HasTotal             (XUnion a)
deriving instance HasTotalWith k a       => HasTotalWith  k      (XUnion a)
deriving instance HasSize a              => HasSize              (XUnion a)
deriving instance CanBeSubset a          => CanBeSubset          (XUnion a)
deriving instance CanBeProperSubset a    => CanBeProperSubset    (XUnion a)


-- Data.Set
instance Ord a => HasUnion (Set.Set a) where
  union = Set.union

instance Ord a => HasDifference (Set.Set a) where
  difference = Set.difference

instance Ord a => HasIntersection (Set.Set a) where
  intersection = Set.intersection

instance HasSingleton a (Set.Set a) where
  singleton = Set.singleton

instance Ord a => HasInsert a (Set.Set a) where
  insert = Set.insert

instance Ord a => HasDelete a (Set.Set a) where
  delete = Set.delete

instance HasEmpty (Set.Set a) where
  empty = Set.empty

instance HasSize (Set.Set a) where
  size = Set.size

instance Ord a => CanBeSubset (Set.Set a) where
  isSubsetOf = Set.isSubsetOf

instance Ord a => CanBeProperSubset (Set.Set a) where
  isProperSubsetOf = Set.isProperSubsetOf


-- Data.Map
instance Ord k => HasUnion (Map.Map k a) where
  union = Map.union

instance Ord k => HasDifference (Map.Map k a) where
  difference = Map.difference

instance Ord k => HasIntersection (Map.Map k a) where
  intersection = Map.intersection

instance HasSingletonWith k a (Map.Map k a) where
  singletonWith = Map.singleton

instance Ord k => HasInsertWith k a (Map.Map k a) where
  insertWith = Map.insert

instance Ord k => HasDelete k (Map.Map k a) where
  delete = Map.delete

instance HasEmpty (Map.Map k a) where
  empty = Map.empty

instance HasSize (Map.Map k a) where
  size = Map.size

instance (Eq k, Ord k, Eq a) => CanBeSubset (Map.Map k a) where
  isSubsetOf = Map.isSubmapOf

instance (Eq k, Ord k, Eq a) => CanBeProperSubset (Map.Map k a) where
  isProperSubsetOf = Map.isProperSubmapOf


-- Data.List
instance HasSingleton a [a] where
  singleton = (:[])

instance HasInsert a [a] where
  insert = (:)

instance Eq a => HasDelete a [a] where
  delete x = List.filter (== x)

instance HasEmpty [a] where
  empty = []

instance HasSize [a] where
  size = List.length

-- Data.Sequence
instance HasSingleton a (Seq.Seq a) where
  singleton = Seq.singleton

instance HasEmpty (Seq.Seq a) where
  empty = Seq.empty

instance HasSize (Seq.Seq a) where
  size = Seq.length

-- Data.IntSet
instance HasUnion IntSet.IntSet where
  union = IntSet.union

instance HasDifference IntSet.IntSet where
  difference = IntSet.difference

instance HasIntersection IntSet.IntSet where
  intersection = IntSet.intersection

instance HasSingleton IntSet.Key IntSet.IntSet where
  singleton = IntSet.singleton

instance HasInsert IntSet.Key IntSet.IntSet where
  insert = IntSet.insert

instance HasDelete IntSet.Key IntSet.IntSet where
  delete = IntSet.delete

instance HasEmpty IntSet.IntSet where
  empty = IntSet.empty

instance HasSize IntSet.IntSet where
  size = IntSet.size

instance CanBeSubset IntSet.IntSet where
  isSubsetOf = IntSet.isSubsetOf

instance CanBeProperSubset IntSet.IntSet where
  isProperSubsetOf = IntSet.isProperSubsetOf


-- Data.IntMap
instance HasUnion (IntMap.IntMap a) where
  union = IntMap.union

instance HasDifference (IntMap.IntMap a) where
  difference = IntMap.difference

instance HasIntersection (IntMap.IntMap a) where
  intersection = IntMap.intersection

instance HasSingletonWith IntMap.Key a (IntMap.IntMap a) where
  singletonWith = IntMap.singleton

instance HasInsertWith IntMap.Key a (IntMap.IntMap a) where
  insertWith = IntMap.insert

instance HasDelete IntMap.Key (IntMap.IntMap a) where
  delete = IntMap.delete

instance HasEmpty (IntMap.IntMap a) where
  empty = IntMap.empty

instance HasSize (IntMap.IntMap a) where
  size = IntMap.size

instance Eq a => CanBeSubset (IntMap.IntMap a) where
  isSubsetOf = IntMap.isSubmapOf

instance Eq a => CanBeProperSubset (IntMap.IntMap a) where
  isProperSubsetOf = IntMap.isProperSubmapOf


-- Data.HashSet
instance (Hashable a, Eq a) => HasUnion (HashSet.HashSet a) where
  union = HashSet.union

instance (Hashable a, Eq a) => HasDifference (HashSet.HashSet a) where
  difference = HashSet.difference

instance (Hashable a, Eq a) => HasIntersection (HashSet.HashSet a) where
  intersection = HashSet.intersection

instance Hashable a => HasSingleton a (HashSet.HashSet a) where
  singleton = HashSet.singleton

instance (Hashable a, Eq a) => HasInsert a (HashSet.HashSet a) where
  insert = HashSet.insert

instance (Hashable a, Eq a) => HasDelete a (HashSet.HashSet a) where
  delete = HashSet.delete

instance HasEmpty (HashSet.HashSet a) where
  empty = HashSet.empty

instance HasSize (HashSet.HashSet a) where
  size = HashSet.size


-- Data.HashMap
instance (Hashable k, Eq k) => HasUnion (HashMap.HashMap k a) where
  union = HashMap.union

instance (Hashable k, Eq k) => HasDifference (HashMap.HashMap k a) where
  difference = HashMap.difference

instance (Hashable k, Eq k) => HasIntersection (HashMap.HashMap k a) where
  intersection = HashMap.intersection

instance Hashable k => HasSingletonWith k a (HashMap.HashMap k a) where
  singletonWith = HashMap.singleton

instance (Hashable k, Eq k) => HasInsertWith k a (HashMap.HashMap k a) where
  insertWith = HashMap.insert

instance (Hashable k, Eq k) => HasDelete k (HashMap.HashMap k a) where
  delete = HashMap.delete

instance HasEmpty (HashMap.HashMap k a) where
  empty = HashMap.empty

instance HasSize (HashMap.HashMap k a) where
  size = HashMap.size

-- Data.SetWith
instance Ord k => HasUnion (SetWith.SetWith k a) where
  union = SetWith.union

instance Ord k => HasDifference (SetWith.SetWith k a) where
  difference = SetWith.difference

instance Ord k => HasIntersection (SetWith.SetWith k a) where
  intersection = SetWith.intersection

instance Ord k => HasSingletonWith (a -> k) a (SetWith.SetWith k a) where
  singletonWith = SetWith.singleton

instance Ord k => HasInsert a (SetWith.SetWith k a) where
  insert = SetWith.insert

instance Ord k => HasDelete a (SetWith.SetWith k a) where
  delete = SetWith.delete

instance HasEmptyWith (a -> k) (SetWith.SetWith k a) where
  emptyWith = SetWith.empty

instance HasSize (SetWith.SetWith k a) where
  size = SetWith.size

instance (Ord k, Eq a) => CanBeSubset (SetWith.SetWith k a) where
  isSubsetOf = SetWith.isSubsetOf

instance (Ord k, Eq a) => CanBeProperSubset (SetWith.SetWith k a) where
  isProperSubsetOf = SetWith.isProperSubsetOf

-- Data.Functor.Contravariant.Predicate
instance HasUnion (Pred.Predicate a) where
  union (Pred.Predicate f) (Pred.Predicate g) = Pred.Predicate $ \x -> f x || g x

instance HasDifference (Pred.Predicate a) where
  difference (Pred.Predicate f) (Pred.Predicate g) = Pred.Predicate $ \x -> f x && not (g x)

instance HasIntersection (Pred.Predicate a) where
  intersection (Pred.Predicate f) (Pred.Predicate g) = Pred.Predicate $ \x -> f x && g x

instance HasComplement (Pred.Predicate a) where
  complement (Pred.Predicate f) = Pred.Predicate $ not . f

instance Eq a => HasSingleton a (Pred.Predicate a) where
  singleton a = Pred.Predicate $ \x -> a == x

instance Eq a => HasInsert a (Pred.Predicate a) where
  insert a (Pred.Predicate f) = Pred.Predicate $ \x -> a == x || f x

instance Eq a => HasDelete a (Pred.Predicate a) where
  delete a (Pred.Predicate f) = Pred.Predicate $ \x -> a /= x && f x

instance HasEmpty (Pred.Predicate a) where
  empty = Pred.Predicate $ const False

instance HasTotal (Pred.Predicate a) where
  total = Pred.Predicate $ const True


-- Data.Set.Ordered.Many
instance Disc.Sorting a => HasUnion (OM.OMSet a) where
  union = OM.union

instance Eq a => HasDifference (OM.OMSet a) where
  difference = OM.difference

instance Ord a => HasIntersection (OM.OMSet a) where
  intersection = OM.intersection

instance HasSingleton a (OM.OMSet a) where
  singleton = OM.singleton

instance Ord a => HasInsert a (OM.OMSet a) where
  insert = OM.insert

instance Eq a => HasDelete a (OM.OMSet a) where
  delete = OM.delete

instance HasEmpty (OM.OMSet a) where
  empty = OM.empty

instance HasSize (OM.OMSet a) where
  size = OM.size

instance Eq a => CanBeSubset (OM.OMSet a) where
  isSubsetOf = OM.isSubsetOf

instance Eq a => CanBeProperSubset (OM.OMSet a) where
  isProperSubsetOf = OM.isProperSubsetOf


-- Data.Set.Unordered.Many
instance Eq a => HasUnion (UM.UMSet a) where
  union = UM.union

instance Eq a => HasDifference (UM.UMSet a) where
  difference = UM.difference

instance Eq a => HasIntersection (UM.UMSet a) where
  intersection = UM.intersection

instance HasSingleton a (UM.UMSet a) where
  singleton = UM.singleton

instance HasInsert a (UM.UMSet a) where
  insert = UM.insert

instance Eq a => HasDelete a (UM.UMSet a) where
  delete = UM.delete

instance HasEmpty (UM.UMSet a) where
  empty = UM.empty

instance HasSize (UM.UMSet a) where
  size = UM.size

instance Eq a => CanBeSubset (UM.UMSet a) where
  isSubsetOf = UM.isSubsetOf

instance Eq a => CanBeProperSubset (UM.UMSet a) where
  isProperSubsetOf = UM.isProperSubsetOf


-- Data.Set.Unordered.Unique
instance Eq a => HasUnion (UU.UUSet a) where
  union = UU.union

instance Eq a => HasDifference (UU.UUSet a) where
  difference = UU.difference

instance Eq a => HasIntersection (UU.UUSet a) where
  intersection = UU.intersection

instance HasSingleton a (UU.UUSet a) where
  singleton = UU.singleton

instance Eq a => HasInsert a (UU.UUSet a) where
  insert = UU.insert

instance Eq a => HasDelete a (UU.UUSet a) where
  delete = UU.delete

instance HasEmpty (UU.UUSet a) where
  empty = UU.empty

instance HasSize (UU.UUSet a) where
  size = UU.size

instance Eq a => CanBeSubset (UU.UUSet a) where
  isSubsetOf = UU.isSubsetOf

instance Eq a => CanBeProperSubset (UU.UUSet a) where
  isProperSubsetOf = UU.isProperSubsetOf


-- Data.Set.Ordered.Unique.Finite
instance Ord a => HasUnion (OUF.FiniteSet a) where
  union = OUF.union

instance Ord a => HasDifference (OUF.FiniteSet a) where
  difference = OUF.difference

instance Ord a => HasIntersection (OUF.FiniteSet a) where
  intersection = OUF.intersection

instance Ord a => HasComplement (OUF.FiniteSet a) where
  complement = OUF.complement

instance HasSingletonWith (Set.Set a) a (OUF.FiniteSet a) where
  singletonWith = OUF.singleton

instance Ord a => HasInsert a (OUF.FiniteSet a) where
  insert = OUF.insert

instance Ord a => HasDelete a (OUF.FiniteSet a) where
  delete = OUF.delete

instance HasEmptyWith (Set.Set a) (OUF.FiniteSet a) where
  emptyWith = OUF.empty

instance HasTotalWith (OUF.FiniteSet a) (OUF.FiniteSet a) where
  totalWith (OUF.FiniteSet (t,_)) = OUF.FiniteSet (t,t)

instance HasSize (OUF.FiniteSet a) where
  size = OUF.size

instance Ord a => CanBeSubset (OUF.FiniteSet a) where
  isSubsetOf = OUF.isSubsetOf

instance Ord a => CanBeProperSubset (OUF.FiniteSet a) where
  isProperSubsetOf = OUF.isProperSubsetOf
